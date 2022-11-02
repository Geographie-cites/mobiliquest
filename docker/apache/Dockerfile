FROM alpine:3.14 as mobiliscope

RUN apk update && apk add bash git

RUN adduser mobiliscope -g "" -D -h /var/mobiliscope/

USER mobiliscope

RUN mkdir -p /var/mobiliscope

RUN cd /var/mobiliscope && git config --global http.sslVerify "false" && \
    git clone https://github.com/Geographie-cites/mobiliscope.git


FROM php:8.0-apache

ENV COMPOSER_ALLOW_SUPERUSER=1

EXPOSE 80
WORKDIR /app/french

# git, unzip & zip are for composer
RUN apt-get update -qq && \
    apt-get install -qy \
    git \
    gnupg \
    unzip \
    zip && \
    curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer && \
    apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# PHP Extensions
RUN docker-php-ext-install -j$(nproc) opcache pdo_mysql
COPY apache/conf/php.ini /usr/local/etc/php/conf.d/app.ini

# Apache
COPY apache/conf/errors /errors
COPY apache/conf/vhost.conf /etc/apache2/sites-available/000-default.conf
COPY apache/conf/apache.conf /etc/apache2/conf-available/z-app.conf
COPY --from=mobiliscope /var/mobiliscope/mobiliscope/www_online/ /app/

RUN a2enmod rewrite remoteip && \
    a2enconf z-app

