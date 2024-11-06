# syntax = docker/dockerfile:1.2
FROM silex/emacs as builder

RUN --mount=type=cache,sharing=locked,target=/var/cache/apt \
    --mount=type=cache,sharing=locked,target=/var/lib/apt \
    apt-get -y update && apt-get --no-install-recommends install -y \
    aspell-en \
    elpa-pdf-tools-server \
    gh \
    git \
    make

ADD . ~/.emacs.d

RUN echo "(custom-set-variables \
    '(use-short-answers t) \
    '(package-native-compile t) \
    '(system-packages-use-sudo nil) \
    )" > ~/.emacs.d/custom.el

RUN --mount=type=cache,sharing=locked,target=~/.emacs.d/elpa \
    yes | emacs --fg-daemon --debug-init --eval "(kill-emacs)"

RUN ls -l ~/.emacs.d/elpa/

RUN make -C ~/.emacs.d/elpa/org-mode compile autoloads

RUN emacs --batch -f batch-byte-recompile-directory ~/.emacs.d/

ENTRYPOINT ["emacs"]
