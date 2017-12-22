from django.contrib import admin
from django.contrib.auth import views as auth_views
from django.urls import path

from .views import Home
from matching.views import ParticipantUpdate

urlpatterns = [
    path('admin/', admin.site.urls),
    path('login/', auth_views.LoginView.as_view(template_name='login.html'), name='login'),
    path('logout/', auth_views.LogoutView.as_view(template_name='logout.html'), name='logout'),
    path('', Home.as_view(), name='home'),
    path('edit/', ParticipantUpdate.as_view(), name='participant-detail'),
]
