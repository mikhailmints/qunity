FROM ocaml/opam:debian-ocaml-5.3 AS build

WORKDIR /qunity

RUN sudo apt-get install -y vim rlwrap python3-venv

COPY dune-project ./
COPY qunity.opam ./

RUN opam install . --deps-only --with-test

COPY requirements.txt ./

RUN sudo python3 -m venv /opt/venv
RUN sudo /bin/bash -c "source /opt/venv/bin/activate && \
    pip install --no-cache-dir --upgrade pip wheel setuptools"
RUN sudo /bin/bash -c "source /opt/venv/bin/activate && \
    pip install --no-cache-dir -r ./requirements.txt"

COPY . .

ENV PATH="/opt/venv/bin:$PATH"

RUN sudo chmod -R 777 .

CMD ["bash"]
