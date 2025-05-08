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
    tzdata

COPY . /root/.emacs.d

RUN echo "(custom-set-variables \
    '(use-short-answers t) \
    '(package-native-compile t) \
    '(system-packages-use-sudo nil) \
    )" > $HOME/.emacs.d/custom.el

RUN --mount=type=cache,sharing=locked,target=$HOME/.emacs.d/elpa \
    --mount=type=cache,sharing=locked,target=$HOME/.emacs.d/eln-cache \
    yes | emacs --fg-daemon --debug-init -kill

RUN --mount=type=cache,sharing=locked,target=$HOME/.emacs.d/elpa \
    --mount=type=cache,sharing=locked,target=$HOME/.emacs.d/eln-cache \
    make -C $HOME/.emacs.d/elpa/org-mode compile autoloads

ENTRYPOINT ["emacs"]
