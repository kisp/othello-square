ARG RUBY_VERSION=3.2.2
FROM ruby:${RUBY_VERSION}

ENV DEBIAN_FRONTEND noninteractive
ENV CHROMIUM_DRIVER_VERSION 113.0.5672.24
ENV CHROME_VERSION 113.0.5672.126-1

SHELL ["/bin/bash", "-o", "pipefail", "-c"]

RUN wget -q -O - https://deb.nodesource.com/setup_20.x | bash - && apt-get update && apt-get -y --no-install-recommends install nodejs

# Install dependencies & Chrome
RUN apt-get update && apt-get -y --no-install-recommends install sbcl zlib1g-dev liblzma-dev wget xvfb unzip libgconf-2-4 libnss3 \
 && wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add -  \
 && echo "deb http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google-chrome.list \
 && apt-get update && apt-get -y --no-install-recommends install google-chrome-stable=$CHROME_VERSION \
 && rm -rf /var/lib/apt/lists/*

# Install Chrome driver
RUN wget -O /tmp/chromedriver.zip http://chromedriver.storage.googleapis.com/$CHROMIUM_DRIVER_VERSION/chromedriver_linux64.zip \
    && unzip /tmp/chromedriver.zip chromedriver -d /usr/bin/ \
    && rm /tmp/chromedriver.zip \
    && chmod ugo+rx /usr/bin/chromedriver \
    && apt-mark hold google-chrome-stable

ADD docker-entrypoint.sh /
ENTRYPOINT ["/docker-entrypoint.sh"]
