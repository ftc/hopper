FROM shawnmeier/cuplv_docker:1
RUN git clone https://github.com/ftc/walautil.git
RUN git clone https://github.com/ftc/droidel.git
RUN git clone https://github.com/ftc/hopper.git

RUN cd walautil \
  && sbt publishLocal

RUN cd droidel \
  && sbt publishLocal

RUN cd hopper \
  && sbt compile

