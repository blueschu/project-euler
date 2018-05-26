#!/usr/bin/env python3
""" Brief script to generate Project Euler problem boilerplate. """

import argparse


def euler_problem_number(arg: str) -> int:
    """
    Argument type checker for Project Euler problem number.

    The arg must be a valid integer in the range [1, 999].

    >>> euler_problem_number('4')
    4
    >>> euler_problem_number('3009')
    Traceback (most recent call last):
        ...
    argparse.ArgumentTypeError: 3009 is not a valid problem number.
    """
    num = int(arg)
    if num not in range(1, 1000):
        raise argparse.ArgumentTypeError('{} is not a valid problem number.'.format(arg))
    return num


def populate_euler_template_file(path: str, problem_number: int) -> str:
    """
    Returns a string of the template file at the specified path populated with the Euler problem_number.
    """
    with open(path, 'r') as fin:
        output = fin.read().format(problem_number=problem_number)
    return output


if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description='Make a templated Project Euler Project Solution.')

    parser.add_argument('problem_number', type=euler_problem_number,
        help='The Project Euler problem number.')
    parser.add_argument('--template', default='template',
        help='Template file to use during solution generation. Defaults to \'template\' if no argument is provided.')
    parser.add_argument('--file-format', default='euler#.py',
        help='The format for the solution file. '
             'All instances of \'#\' will be replaced with the three-digit problem number')

    args = parser.parse_args()
    file_name = args.file_format.replace('#', '{0:03d}'.format(args.problem_number))
    output = populate_euler_template_file(args.template, args.problem_number)

    with open(file_name, 'x') as fout:
        fout.write(output)
