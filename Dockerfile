FROM debian:stretch

WORKDIR /opt/jambda

COPY . /opt/jambda

# Install deps
RUN apt-get update && \
    apt-get install -y sudo curl libncurses5-dev libncursesw5-dev pkg-config && \
    apt-get clean

# Install stack
RUN curl -sSL https://get.haskellstack.org/ | sh

# Install SDL2
RUN sudo apt-get install -y libsdl2-dev && \
    apt-get clean

# Build the app
RUN stack build

# Run the app
CMD ["stack", "exec", "Jambda-exe"]
