# syntax = docker/dockerfile:1.2
FROM archlinux

RUN echo -e "[archlinuxcn]\nServer = https://mirrors.cernet.edu.cn/archlinuxcn/\$arch" >> /etc/pacman.conf
RUN echo "Server = https://mirrors.cernet.edu.cn/archlinux/\$repo/os/\$arch" > /etc/pacman.d/mirrorlist

RUN --mount=type=cache,sharing=locked,target=/var/cache/pacman \
    gpgconf --kill gpg-agent && \
    rm -rf /etc/pacman.d/gnupg && \
    pacman-key --init && \
    pacman-key --populate && \
    pacman -Syyu --noconfirm --needed \
    archlinux-keyring \
    archlinuxcn-keyring

RUN --mount=type=cache,sharing=locked,target=/var/cache/pacman \
    pacman -Syyu --noconfirm --needed \
    aspell-en \
    git \
    github-cli \
    gnupg \
    ripgrep \
    emacs-wayland

RUN emacs --version

ADD . /root/.emacs.d

RUN echo "(custom-set-variables '(use-short-answers t))" > /root/.emacs.d/custom.el

RUN --mount=type=cache,sharing=locked,target=/root/.emacs.d/elpa \
    yes | emacs --fg-daemon --debug-init

ENTRYPOINT ["emacs", "-nw"]
