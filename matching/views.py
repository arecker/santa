from django.contrib.auth.decorators import login_required
from django.urls import reverse_lazy
from django.utils.decorators import method_decorator
from django.views.generic.edit import UpdateView

from .models import Participant


@method_decorator(login_required, name='dispatch')
class ParticipantUpdate(UpdateView):
    model = Participant
    fields = [
        'address_1', 'address_2',
        'city', 'state', 'province', 'country',
        'zip_code', 'notes',
    ]
    success_url = reverse_lazy('home')

    def get_object(self, *args, **kwargs):
        return Participant.objects.get(user=self.request.user)
