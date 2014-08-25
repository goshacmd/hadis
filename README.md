# Hadis

A Haskell implementation of Redis server. **WIP**.

```bash
$ hadis
> KEYS "*"
["a"]
> GET "a"
"123"
> GETSET "a" "777"
"123"
> GET "a"
"777"
>
```

## License

[MIT](LICENSE).
