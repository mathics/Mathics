=========
Releasing
=========


Announce
========

Let folks know in advance about the release. During the release of
course, ensure no one is changing master (except the person doing the
release).

Get latest sources:
===================

::

    $ git pull

Change version in mathics/version.py
====================================

On mathics/Mathics master branch. For each release candidate:

update ``__version__`` in ``mathics/version.py``

::

    $ source mathics/version.py # to set in POSIX shell
    $ echo $__version__
    $ git commit -m"Get ready for release $__version__" .

Update Changes
==============

::

    $ make ChangeLog

Update ``CHANGES.rst`` from ``ChangeLog``

::

    $ make check
    $ git commit --amend .
    $ git push   # get CI testing going early

http://rst.ninjs.org/ can be used for checking the RsT.

Build Docs
==========

::

    $ make doc

See also [[Documentation and tests]].

Check package from github
=========================

Todo: turn this into a script in ``admin-tools``

::

    $ [[ ! -d /tmp/gittest ]] && mkdir /tmp/gittest; pushd /tmp/gittest
    $ pyenv local 3.7.7  # Use a version that is not the most recent
    $ pip install -e git://github.com/mathics/Mathics.git#egg=mathics
    $ mathics --version # see that new version appears
    $ mathics -e "1+2"
    $ mathicsserver
    $ pip uninstall mathics3
    $ popd

Make packages and check
=======================

::

    $ (cd ./admin-tools && bash ./make-dist.sh)
    $ twine check dist/Mathics3-$__version__*

Release on Github
=================

Goto https://github.com/mathics/Mathics/releases/new

https://cloudconvert.com/rst-to-md can be used to change the CHANGES.rst
section to markdown.

Now check the *tagged* release. (Checking the untagged release was
previously done).

Todo: turn this into a script in ``admin-tools``

::

    $ git pull # to pull down new tag
    $ pushd /tmp/gittest
    $ pyenv local 3.7.7 # Use a version that is not the most recent
    $ pip install -e git://github.com/mathics/Mathics.git@${__version__}#egg=mathics
    $ mathics --version
    $ mathics -e "1+2"
    $ mathicsserver
    $ pip uninstall mathics3
    $ popd

Upload the release to PyPI
==========================

Upload it to PyPI with twine

.. code:: bash

    $ twine upload dist/Mathics3-${__version__}*

Post-Release
============

Update mathicsscript
--------------------

Update docker setup
-------------------

::

      $ make docker-image
      $ docker run --rm -it --name mathics-cli -v /tmp:/usr/src/app/data mathicsorg/mathics --mode cli -- --version
      $ docker run --rm -it --name mathics-cli -v /tmp:/usr/src/app/data mathicsorg/mathics --mode cli -- -e 1+2
      $ docker tag mathicsorg/mathics:latest  mathicsorg/mathics:${__version__}

Check https://hub.docker.com/repository/docker/mathicsorg/mathics

Update magic.github.io PDF
--------------------------

::

       $ cp mathics/doc/tex/mathics.pdf ../mathics.github.io/docs/mathics-${__version__}.pdf
       $ cd ../mathics.github.io/docs
       $ git add mathics-${__version__}.pdf
       $ rm mathics-latest.pdf
       $ ln -vs mathics-${__version__}.pdf mathics-latest.pdf
       $ git commit .
       $ git push

-  Announce release on Google-Groups pages
-  https://groups.google.com/forum/#!forum/mathics-devel,
-  https://groups.google.com/forum/#!forum/mathics-users, and
-  https://groups.google.com/g/sage-develsage-users
-  Update ``__version__`` to ``NEXT_VERSION.dev0``
