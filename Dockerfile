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
RUN mkdir -p $HOME/.config/emacs/templates
COPY templates/custom.el /root/.config/emacs/templates/custom.el

RUN --mount=type=cache,sharing=locked,target=$HOME/.config/emacs/elpa \
    --mount=type=cache,sharing=locked,target=$HOME/.config/emacs/eln-cache \
    yes | emacs --batch --debug-init \
    --init-directory "$HOME/.config/emacs" \
    -l "$HOME/.config/emacs/early-init.el" \
    -l "$HOME/.config/emacs/init.el" \
    --eval "(package-initialize)" \
    --eval "(package-refresh-contents)" \
    --eval "(package-install-selected-packages t)" \
    --eval "(package-autoremove)"

COPY . /root/.config/emacs

RUN --mount=type=cache,sharing=locked,target=$HOME/.config/emacs/elpa \
    --mount=type=cache,sharing=locked,target=$HOME/.config/emacs/eln-cache \
    make -C $HOME/.config/emacs/elpa/org-mode compile autoloads

ENTRYPOINT ["emacs"]
