#!/usr/bin/env node
/**
 * Claude Code Stop hook that analyzes transcripts for memory-worthy content.
 * Runs after each Claude response and suggests additions to AGENTS.override.md
 *
 * Usage: Configured as a Stop hook in ~/.claude/settings.json
 * Input: JSON via stdin with { transcript_path, cwd, stop_hook_active, ... }
 * Output: JSON with suggested memory additions (or empty to allow stop)
 */

import Anthropic from "@anthropic-ai/sdk";
import { readFileSync, existsSync, appendFileSync } from "fs";
import { join } from "path";

// Debug logging (per-session file)
let debugFile = null;
function debug(msg, data = null) {
  if (!debugFile) return;
  const timestamp = new Date().toISOString();
  let line = `[${timestamp}] ${msg}`;
  if (data !== null) {
    line += `: ${JSON.stringify(data)}`;
  }
  appendFileSync(debugFile, line + "\n");
}

// Configuration
const MEMORY_FILENAME = "AGENTS.override.md";
const MODEL = "claude-opus-4-5-20251101";
const MAX_TRANSCRIPT_CHARS = 100000; // Limit transcript size to control costs
const MIN_TRANSCRIPT_CHARS = 500; // Skip very short conversations

async function main() {
  try {
    // Read hook input from stdin
    const input = readFileSync(0, "utf-8");
    const hookData = JSON.parse(input);

    // Set up per-session debug file
    const sessionId = hookData.session_id || "unknown";
    debugFile = `/tmp/extract-memories-${sessionId}.log`;
    debug("Hook started", { stop_hook_active: hookData.stop_hook_active });

    // Skip if this is a continuation from a previous stop hook (prevent loops)
    if (hookData.stop_hook_active) {
      debug("SKIP: stop_hook_active");
      process.exit(0);
    }

    const { transcript_path, cwd } = hookData;
    if (!transcript_path || !existsSync(transcript_path)) {
      debug("SKIP: no transcript", { transcript_path });
      process.exit(0);
    }

    if (!cwd) {
      debug("SKIP: no cwd");
      process.exit(0);
    }

    // Find the memory file in the project root
    const memoryFile = join(cwd, MEMORY_FILENAME);

    // Read and parse transcript (JSONL format)
    const transcriptContent = readFileSync(transcript_path, "utf-8");
    const messages = transcriptContent
      .split("\n")
      .filter((line) => line.trim())
      .map((line) => {
        try {
          return JSON.parse(line);
        } catch {
          return null;
        }
      })
      .filter(Boolean);

    if (messages.length === 0) {
      debug("SKIP: no messages");
      process.exit(0);
    }
    debug("Parsed messages", { count: messages.length });

    // Format transcript for analysis
    const formattedTranscript = formatTranscript(messages);
    if (formattedTranscript.length < MIN_TRANSCRIPT_CHARS) {
      debug("SKIP: transcript too short", {
        length: formattedTranscript.length,
        min: MIN_TRANSCRIPT_CHARS,
      });
      process.exit(0);
    }
    debug("Formatted transcript", { length: formattedTranscript.length });

    // Read current memory file (if it exists)
    const currentMemories = existsSync(memoryFile)
      ? readFileSync(memoryFile, "utf-8")
      : "";

    // Analyze with Opus
    debug("Calling Anthropic API");
    const suggestions = await analyzeForMemories(
      formattedTranscript,
      currentMemories,
      cwd,
    );
    debug("API returned", {
      hasSuggestions: !!(suggestions && suggestions.trim()),
    });

    if (suggestions && suggestions.trim()) {
      debug("Outputting suggestions");
      // Output suggestion to user via hook output
      console.log(
        JSON.stringify({
          systemMessage: `\n📝 **Memory Suggestions** (from transcript analysis):\n\n${suggestions}\n\nTo add these to ${MEMORY_FILENAME}, say "update my memories" or manually add them.`,
        }),
      );
    }

    debug("Hook completed");
    process.exit(0);
  } catch (error) {
    debug("ERROR", { message: error.message });
    // Silent failure - don't block Claude from stopping
    if (process.env.CLAUDE_HOOK_DEBUG) {
      console.error("[extract-memories]", error.message);
    }
    process.exit(0);
  }
}

function formatTranscript(messages) {
  let result = "";

  for (const msg of messages) {
    if (msg.type === "human" || msg.type === "user") {
      // msg.message is { role: "user", content: "string" }
      const content = extractTextContent(msg.message?.content);
      if (content) {
        result += `\n## USER:\n${content}\n`;
      }
    } else if (msg.type === "assistant" || msg.type === "ai") {
      // msg.message is { role: "assistant", content: [{ type: "text", text: "..." }, ...] }
      const content = extractTextContent(msg.message?.content);
      if (content) {
        result += `\n## ASSISTANT:\n${content}\n`;
      }
    }
  }

  // Truncate if too long (keep the end, which is most recent)
  if (result.length > MAX_TRANSCRIPT_CHARS) {
    result = result.slice(-MAX_TRANSCRIPT_CHARS);
    result = "...[truncated]...\n" + result;
  }

  return result;
}

function extractTextContent(content) {
  if (typeof content === "string") {
    return content;
  }
  if (Array.isArray(content)) {
    return content
      .filter((block) => block.type === "text")
      .map((block) => block.text)
      .join("\n");
  }
  return "";
}

async function analyzeForMemories(transcript, currentMemories, projectPath) {
  const client = new Anthropic();

  const systemPrompt = `You are analyzing a Claude Code conversation transcript to identify insights worth remembering for future sessions.

Project path: ${projectPath}

Your task: Extract actionable, reusable knowledge that would help in future coding sessions with this codebase.

## What makes a good memory:
- Discovered patterns, conventions, or architectural decisions specific to THIS codebase
- Non-obvious configuration or setup requirements
- Common pitfalls and their solutions
- Workflow shortcuts or preferences the user expressed
- Important file locations or code organization insights

## What to SKIP:
- Generic programming knowledge (e.g., "use async/await")
- One-time fixes that won't recur
- Information already in the current memories
- Obvious facts about well-known libraries
- Verbose explanations - be concise
- Anything that was just a quick question/answer without lasting value

## Current ${MEMORY_FILENAME} contents:
${currentMemories || "(empty)"}

CRITICAL: Before suggesting any memory, check if it duplicates or overlaps with existing content above. Only suggest genuinely NEW insights not already captured.

## Output format:
If there are memories worth adding, output ONLY the markdown content ready to append to ${MEMORY_FILENAME}.
Use appropriate headers (## or ###) that fit the existing structure.
Do NOT include any explanation or preamble - just the raw markdown to append.
If nothing is worth remembering, output ONLY the word: NO_MEMORIES

Be selective - only suggest truly valuable additions. Quality over quantity.`;

  const response = await client.messages.create({
    model: MODEL,
    max_tokens: 1024,
    messages: [
      {
        role: "user",
        content: `Analyze this conversation transcript and extract any memories worth saving:\n\n${transcript}`,
      },
    ],
    system: systemPrompt,
  });

  const result = response.content
    .filter((block) => block.type === "text")
    .map((block) => block.text)
    .join("\n")
    .trim();

  if (result.includes("NO_MEMORIES")) {
    return null;
  }

  return result;
}

main();
