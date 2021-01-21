#!/bin/sh
docker run --rm -p 8000:8000 -v `C:/work/veit06/Schulungen/isaqb_flex/flex-arvato/`:/monolithic -w /monolithic -i -t erlang-flex bash
