# syntax = docker/dockerfile:1.2
FROM archlinux

RUN --mount=type=cache,sharing=locked,target=/var/cache/pacman \
    gpgconf --kill gpg-agent && \
    rm -rf /etc/pacman.d/gnupg && \
    pacman-key --init && \
    pacman-key --populate && \
    pacman -Syyu --noconfirm --needed \
    archlinux-keyring

RUN --mount=type=cache,sharing=locked,target=/var/cache/pacman \
    pacman -Syyu --noconfirm --needed \
    aspell-en \
    git \
    github-cli \
    gnupg \
    p7zip \
    ripgrep \
    emacs-wayland

RUN emacs --version

ADD . /root/.emacs.d

RUN echo "(custom-set-variables '(use-short-answers t))" > /root/.emacs.d/custom.el

RUN --mount=type=cache,sharing=locked,target=/root/.emacs.d/elpa \
    yes | emacs --fg-daemon --debug-init --eval "(kill-emacs)"

CMD ["emacs"]
