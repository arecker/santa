from django.contrib.auth.models import User
from django.db import models
from django.urls import reverse

from threadlocalrequest import threadlocals


def everyone_but_me():
    user = threadlocals.get_current_request().user
    return Participant.objects.exclude(user=user)


class Participant(models.Model):
    user = models.OneToOneField(User, on_delete=models.CASCADE)
    address_1 = models.CharField(max_length=280, blank=True, null=True)
    address_2 = models.CharField(max_length=280, blank=True, null=True)
    city = models.CharField(max_length=280, blank=True, null=True)
    state = models.CharField(max_length=2, blank=True, null=True)
    province = models.CharField(max_length=280, blank=True, null=True)
    country = models.CharField(max_length=280, blank=True, null=True)
    zip_code = models.CharField(max_length=280, blank=True, null=True)
    notes = models.TextField(blank=True, null=True)

    default_exclusions = models.ManyToManyField(
        'Participant', blank=True, limit_choices_to=everyone_but_me
    )

    def __str__(self):
        return self.user.get_full_name() or self.user.get_username()

    def get_absolute_url(self):
        return reverse('participant-detail', kwargs={'pk': self.pk})
