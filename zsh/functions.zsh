# Configure version of Java used.
if [[ "$OSTYPE" == darwin* ]]; then
  java_use() {
    export JAVA_HOME=$(/usr/libexec/java_home -v $1)
  }
fi
