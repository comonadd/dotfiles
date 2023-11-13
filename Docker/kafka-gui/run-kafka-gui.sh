#!/bin/bash


PORT=4883

echo "Running Kafka GUI on port $PORT"

docker run -p $PORT:8080 \
    -e KAFKA_BROKERS=$KAFKA_BROKERS \
    -e KAFKA_SASL_ENABLED=true \
    -e KAFKA_SASL_MECHANISM=$KAFKA_SASL_MECHANISM \
    -e KAFKA_SASL_USERNAME=$KAFKA_USERNAME \
    -e KAFKA_SASL_PASSWORD=$KAFKA_PASSWORD \
    docker.redpanda.com/redpandadata/console:latest
