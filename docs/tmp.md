This guide descrbes how to setup Mathics on a local network. There are additional (security) considerations for running Mathics on a publically facing webserver.

Best practises for a local network
===================================

- Install PyPy
```
sudo apt-get install pypy
```

- Install Setuptools
```
curl -O http://peak.telecommunity.com/dist/ez_setup.py
pypy ez_setup.py
```

- Download and Install Mathics
```
curl -L  -O https://github.com/mathics/Mathics/releases/download/v0.8/mathics-0.8.tar.gz`
tar xzf mathics-0.8.tar.gz
cd mathics-0.8/
sudo pypy setup.py install
```

You can now run the web server with `mathicsserver -e` but you probably want to make some changes first.
- disable the files module by setting `ENABLE_FILES_MODULE = False` in `mathics/settings.py` (otherwise remote users will be able to read and write local files).
- set an execution timeout in `mathics/setttings.py`, e.g. `TIMEOUT = 10` for a 10s limit. 
- Various other changes in the `settings.py` file like email addresses.

You probably also want to run the server as a restricted user within a jail shell

Running Mathics on a public webserver
-------------------------------------
**Warning:** You should be very careful running Mathics publicly, there are some potentially large security implications to be aware of!

The setup is similar but you can use ngnix to cache the static content. Mathics runs as a wsgi app so you can use uwsgi. The [Django docs](https://uwsgi-docs.readthedocs.org/en/latest/tutorials/Django_and_nginx.html) are a good reference.
