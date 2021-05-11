"""
Fileformats

Importers / Exporters use some internal routines/ Symbols to accomplish their task. Submodules
in this folder contain these routines. The context of the corresponding symbols are subcontexts of
System`, like System`HTML` or System`XML to avoid spoil the System` namespace.


"""

from mathics.version import __version__  # noqa used in loading to check consistency.
