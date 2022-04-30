# Hack Compiler

## Usage

### Preparation

TO build this compiler, you have to pass a GitHub token because this compiler
depends on [pois0/pg4scala](https://github.com/pois0/pg4scala),
whose package is hosted on GitHub packages.

The simplest way is to add it as an environment variable. Or you can also set it
in `.gitconfig`.

Example:

```toml
[github]
    token = ${YOUR_TOKEN}
    user = ${YOUR_ID}
```

Of course the followings are also required.

- \>= JDK 10
- sbt

### Execute

```shell
sbt run $PATH_TO_JACK_FILE
# or
sbt run $PATH_TO_SRC_DIR
```

## License

This project is licensed under Apache License 2.0.
