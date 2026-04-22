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

COPY . /root/.config/emacs

RUN echo "(custom-set-variables \
    '(use-short-answers t) \
    '(package-native-compile t) \
    '(system-packages-use-sudo nil) \
    )" > $HOME/.config/emacs-custom.el

RUN --mount=type=cache,sharing=locked,target=$HOME/.config/emacs/elpa \
    --mount=type=cache,sharing=locked,target=$HOME/.config/emacs/eln-cache \
    yes | emacs --fg-daemon --debug-init -kill

RUN --mount=type=cache,sharing=locked,target=$HOME/.config/emacs/elpa \
    --mount=type=cache,sharing=locked,target=$HOME/.config/emacs/eln-cache \
    make -C $HOME/.config/emacs/elpa/org-mode compile autoloads

ENTRYPOINT ["emacs"]
