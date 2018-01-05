#!/usr/bin/env python

# File: timer.py
# Creation date: 11 Jul 2017
# Creator: Dmitry Guzeev <dmitry.guzeev@yahoo.com>
# Description:
# A simple timer for command-line usage

import sys
import time

def print_help():
    """Print the help message."""

    title = "The simple timer"
    print(title)
    print("-" * len(title))
    print("Usage:")
    print("{} <the_amount_of_minutes_to_wait>".format(sys.argv[0]))

def print_help_and_exit():
    """Print the help message and exit."""

    print_help()
    sys.exit(1)

def main():
    """The script entry point."""

    # Check the arguments
    if len(sys.argv) != 2:
        print_help_and_exit()

    # Calculate the seconds to wait
    minutes_to_wait = int(sys.argv[1])
    seconds_to_wait = minutes_to_wait * 60
    print("Waiting {} seconds".format(seconds_to_wait))

    # Save the start time
    start = time.time()

    # The actual loop
    waited = 0
    while waited < seconds_to_wait:
        time.sleep(1)
        waited = waited + 1

    print("Done")

# Enter the main function
if __name__ == "__main__":
    main()
