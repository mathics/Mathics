import mathics
from subprocess import call


def main():
    call([
        'jupyter', 'console', '--kernel', 'mathics',
        '--ZMQTerminalInteractiveShell.banner1=' + mathics.version_string,
        '--ZMQTerminalInteractiveShell.banner2=' + mathics.license_string,
    ])

if __name__ == '__main__':
    main()
