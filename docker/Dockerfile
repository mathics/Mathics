FROM python:3.8-buster

ENV MATHICS_HOME=/usr/src/app
ENV ENTRYPOINT_COMMAND="docker run -it {MATHICS_IMAGE}"

WORKDIR $MATHICS_HOME

COPY entrypoint.sh /
RUN chmod +x /entrypoint.sh

COPY requirements.txt ./
RUN apt-get update
RUN apt-get install -qq liblapack-dev llvm-dev gfortran maria
RUN pip install --no-cache-dir -r requirements.txt
COPY requirements-mathicsscript.txt ./
RUN pip install --no-cache-dir -r requirements-mathicsscript.txt

RUN pip install pymathics-natlang
RUN pip install pymathics-graph
RUN pip install mathicsscript
RUN python -m nltk.downloader wordnet omw
RUN python -m spacy download en
RUN pip install -e git://github.com/mathics/Mathics@remove-frontends#egg=Mathics3
RUN pip install -e git://github.com/mathics3/mathics-django.git#egg=mathics_django

EXPOSE 8000

RUN groupadd mathics && \
    useradd -d $MATHICS_HOME -g mathics -m -s /bin/bash mathics && \
    mkdir -p $MATHICS_HOME/data && \
    chown -R mathics:mathics $MATHICS_HOME

USER mathics

ENTRYPOINT ["/entrypoint.sh"]

CMD ["--help"]
