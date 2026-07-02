# syntax = docker/dockerfile:1.2
FROM silex/emacs as builder
ARG CI
ENV CI=$CI

RUN --mount=type=cache,sharing=locked,target=/var/cache/apt \
    --mount=type=cache,sharing=locked,target=/var/lib/apt \
    apt-get -y update && apt-get --no-install-recommends install -y \
    aspell-en \
    elpa-pdf-tools-server \
    gh \
    git \
    make \
    p7zip \
    tzdata

COPY early-init.el /root/.config/emacs/early-init.el
COPY init.el /root/.config/emacs/init.el
COPY core /root/.config/emacs/core
COPY Makefile /root/.config/emacs/Makefile
RUN mkdir -p $HOME/.config/emacs/templates
COPY templates/custom.el /root/.config/emacs/templates/custom.el

RUN --mount=type=cache,sharing=locked,target=$HOME/.config/emacs/elpa \
    --mount=type=cache,sharing=locked,target=$HOME/.config/emacs/eln-cache \
    make -C $HOME/.config/emacs sync-packages

COPY . /root/.config/emacs

RUN --mount=type=cache,sharing=locked,target=$HOME/.config/emacs/elpa \
    --mount=type=cache,sharing=locked,target=$HOME/.config/emacs/eln-cache \
    make -C $HOME/.config/emacs compile-org

ENTRYPOINT ["emacs"]
