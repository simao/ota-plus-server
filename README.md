# OTA Plus

To quickly get a dev system up and running, use docker-compose:

```
cd docker
docker-compose -f common.yml -f precise.yml up
```

You can edit `precise.yml` with the particular image tags you want.
Wait until the migrations have finished running before you actually use the
server.

Once everything is loaded, the OTA Plus admin GUI will be available at
`{docker-host}:8000`.
