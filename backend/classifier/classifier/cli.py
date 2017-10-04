import click
from .commands.classify import classify
from .commands.build import build
from .commands.seed import seed
from .commands.diagnostics import diagnostics

@click.group()
@click.option("--base", default="data", help="Path to the data directory")
@click.pass_context
def cli(ctx, base):
    """
    classify: run various tasks for classification
    """
    ctx.obj["base"] = click.format_filename(base)

cli.add_command(classify)
cli.add_command(build)
cli.add_command(seed)
cli.add_command(diagnostics)
