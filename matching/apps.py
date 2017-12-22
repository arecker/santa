from django.apps import AppConfig


class MatchingConfig(AppConfig):
    name = 'matching'
    
    def ready(self):
        import .signals
