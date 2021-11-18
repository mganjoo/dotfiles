#!/usr/bin/env python3

import asyncio
import iterm2
from Foundation import NSUserDefaults
# To install, update, or remove packages from PyPI, use Scripts > Manage > Manage Dependencies...

# Color presets to use
LIGHT_PRESET_NAME="Solarized Light"
DARK_PRESET_NAME="Solarized Dark"

# Profiles to update
PROFILES=["Default"]


def get_preset():
    style = NSUserDefaults.standardUserDefaults().stringForKey_("AppleInterfaceStyle")
    if style == "Dark":
        return DARK_PRESET_NAME
    else:
        return LIGHT_PRESET_NAME

async def set_colors(connection, preset_name):
    preset = await iterm2.ColorPreset.async_get(connection, preset_name)
    for partial in (await iterm2.PartialProfile.async_query(connection)):
        if partial.name in PROFILES:
            await partial.async_set_color_preset(preset)

async def main(connection):
    current_preset_name = None
    while True:
        preset_name = get_preset()
        if preset_name != current_preset_name:
            print("Change to preset {}".format(preset_name))
            await set_colors(connection, preset_name)
            current_preset_name = preset_name
        await asyncio.sleep(3)

# This instructs the script to run the "main" coroutine and to keep running even after it returns.
iterm2.run_forever(main)
