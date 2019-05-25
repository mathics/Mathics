#!/usr/bin/env python
# -*- coding: utf-8 -*-


from django.db import models
from django.contrib.auth.models import User


class Query(models.Model):
    time = models.DateTimeField(auto_now_add=True)
    query = models.TextField()
    result = models.TextField(null=True)
    timeout = models.BooleanField()
    out = models.TextField()
    error = models.BooleanField()

    remote_user = models.CharField(max_length=255, null=True)
    remote_addr = models.TextField(null=True)
    remote_host = models.TextField(null=True)
    browser = models.TextField(null=True)
    meta = models.TextField(null=True)

    log = models.TextField(null=True)


class Worksheet(models.Model):
    user = models.ForeignKey(User, related_name='worksheets', null=True)
    name = models.CharField(max_length=30)
    content = models.TextField()

    class Meta:
        unique_together = (('user', 'name'),)
