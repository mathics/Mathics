# -*- coding: utf-8 -*-
from .helper import session
from mathics.session import load_default_settings_files, get_settings_value


def test_settings():
    load_default_settings_files(session.definitions)

    assert get_settings_value(session.definitions, "NoSettingHere") == None
    assert type(session.evaluate("Settings`$TraceGet").to_python()) is bool

    assert (
        session.evaluate("Settings`$TraceGet::usage").to_python()
        != "Settings`$TraceGet::usage"
    )

    assert (
        session.evaluate("Settings`$PreferredBackendMethod::usage").to_python()
        != "Settings`$PreferredBackendMethod::usage"
    )

    assert type(session.evaluate("Settings`$PreferredBackendMethod").to_python()) is str

    assert (
        session.evaluate("System`$Notebooks::usage").to_python()
        != "System`$Notebooks::usage"
    )


def test_is_not_notebook():
    # the settings already were loaded
    assert session.evaluate("System`$Notebooks").to_python() == False
