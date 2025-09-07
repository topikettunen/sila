# The Sila programming language

> śīla (Pali: sīla; T. tshul khrims ཚུལ་ཁྲིམས་), literally, ‘acting
> appropriately’. Sila is translated as "skillful conduct," "discipline,"
> "ethical conduct," "moral conduct," "virtue," etc.
>
> Sila is said to be a way of being that is conducive to positive and happy
> states of mind.
>
> With the Buddhist teachings, sila is identified as:
>
> - one of the three trainings
> - one of the six paramitas in the Sanskrit tradition
> - one of the ten paramis in the Pali tradition
>
> From https://encyclopediaofbuddhism.org/wiki/Śīla

Sila is a programming language that, like all new languages, has somewhat
*optimistic aspirations*.

Language is implemented by a single person, mainly for educational and
research purposes. Very much work in progress. Fully developed in the open.

I'm writing about the development of this language to my blog when something
interesting gets implemented. If you're interested about it, follow me
[here](https://topikettunen.com/blog/tag/sila).

This compiler originally was written and prototyped with Common Lisp but since
it has been rewritten in C++ using LLVM as backend. If you're interested in the
old Common Lisp backend, it can be found [here](https://github.com/topikettunen/cl-sila).

## Build

```
$ cmake -Bbuild -GNinja -DSILA_BUILD_TESTS=1 .
$ cd build
$ ninja

# Run tests
$ /bin/testsila

# Start REPL
$ /bin/sila
```

## Author

* Topi Kettunen <topi@topikettunen.com>

## Copyright

Copyright (c) Topi Kettunen <topi@topikettunen.com>

## License

Licensed under the [MIT License](LICENSE).
