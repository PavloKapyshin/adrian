import copy

from . import parser, foreign_parser
from . import context
from .env import Env


def compile_(inp):
    with context.new_context(env=Env(), exit_on_error=False):
        return foreign_parser.main(parser.Parser().parse(inp))