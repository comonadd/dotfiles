#!/bin/bash

PORT=4883

echo "Running Kafka GUI on port $PORT"

docker run -p $PORT:8080 --env-file=.env.preprod docker.redpanda.com/redpandadata/console:latest
