ARG VARIANT="latest"
FROM erlang:${VARIANT}

# ** [Optional] Uncomment this section to install additional packages. **
#RUN apk update \
#    && apk add --no-cache git make openssl-dev
RUN apt update && apt install -y build-essential groff

# ARG USERNAME=vscode
# ARG USER_UID=1000
# ARG USER_GID=1001
# ARG GRPNAME=vscodegrp

# ENV DEBIAN_FRONTEND=noninteractive

# RUN addgroup --gid $USER_GID $GRPNAME \
#     && adduser --uid $USER_UID -gid $USER_GID $USERNAME
# USER $USERNAME