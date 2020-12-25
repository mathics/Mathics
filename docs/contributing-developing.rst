Contributing & Developing
=========================

Contributing
------------

Here is the basic workflow for contributions to the Mathics source code:

1. Create your own github account and fork the `Mathics github
   repository <https://github.com/mathics/Mathics>`__ (see `github help
   on forking <https://help.github.com/articles/fork-a-repo>`__ which
   also includes explanations of further steps).

2. Clone your github repository to your local machine, edit the code,
   make commits. git is different to other version control systems such
   as SVN in the way that the whole repository (with its whole history)
   is also stored on your local machine. Therefore, commits are not the
   final steps to publish something, but rather atomic steps during
   development.

3. Push your local changes to your upstream github repository. Do this
   regularly to have backups and to enable others to view what you're
   working on. Your changes will still not be in the main Mathics
   repository (unless you have push access).

4. Test your code! Run ``make test`` to run the whole Mathics test
   suite. Use ``python mathics/test.py -s SYMBOL`` regularly to run all
   test cases for a specific ``SYMBOL``. You might also want to `build
   the documentation
   PDF <https://github.com/mathics/Mathics/wiki/Building-the-documentation>`__
   to see how it is affected by your changes to documentation.

5. Make a github pull request to have your code integrated in the main
   repository (see the `help on pull
   requests <https://help.github.com/articles/using-pull-requests>`__).
   After your code has been successfully reviewed, it will officially be
   part of Mathics. Once you open a pull request some automated tests
   will run on Travis CI. If one of these tests fail click on the
   details link and try to figure out why.

6. Every now and then, a version of Mathics is released, i.e., packaged
   and uploaded. This is usually the time when the code running on the
   Mathics mirrors is updated as well.

There are various tools for working with git, e.g.,
`EGit <http://www.eclipse.org/egit/>`__ for
`Eclipse <http://www.eclipse.org/>`__. However, using git on the console
is also convenient (and sometimes a lot faster).

This was only a short outline of the usual git workflow. For a more
complete reference, see the `Git Reference <http://gitref.org/>`__ or
others. One powerful feature of git is branching - you should check it
out.

Before, while, and after working on something, write to
`mathics-devel <https://groups.google.com/forum/?fromgroups#!forum/mathics-devel>`__
to give other developers the chance to comment on your work and to help
you.

Thanks for your contributions, and happy coding!

Developing
----------

To start developing, check out the source directory. Run

::

    $ python setup.py develop

This will temporarily overwrite the installed package in your Python
library with a link to the current source directory. In addition, you
might want to start the Django development server with

::

    $ python manage.py runserver

or

::

    $ make runserver

It will restart automatically when you make changes to the source code.
