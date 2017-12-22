FROM python:3.6
MAINTAINER Alex Recker <alex@reckerfamily.com>

ADD requirements.txt /tmp/
RUN pip install -r /tmp/requirements.txt