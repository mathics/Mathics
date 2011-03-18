# -*- coding: UTF-8 -*-

u"""
    Mathics: a general-purpose computer algebra system
    Copyright (C) 2011 Jan Pöschko

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
"""

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