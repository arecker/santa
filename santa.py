import os
import random
import smtplib

import yaml


EMAIL_TEMPLATE = '''
Greetings {name},

I have your secret santa match!

Name: {match_name}
Address: {match_address}

Remember, you need to ship your gift by {deadline} and it should be
around {price_limit}

Merry Christmas,

Santa Bot
https://github.com/arecker/santa
'''


class ShakeAgain(Exception):
    pass


def read_config():
    here = os.path.dirname(os.path.abspath(__file__))
    with open(os.path.join(here, 'config.yml'), 'r') as f:
        return yaml.load(f)


def get_pairs(names, participant_data):
    pairs = {}

    for i, name in enumerate(names):
        try:
            next_name = names[i + 1]
        except IndexError:
            next_name = names[0]

        if next_name in participant_data[name].get('exclude', []):
            raise ShakeAgain

        pairs[name] = next_name

    return pairs


def send_emails(pairs, config):
    for person, match in pairs.items():

        body = EMAIL_TEMPLATE.format(
            name=config['participants'][person]['name'],
            match_name=config['participants'][match]['name'],
            match_address=config['participants'][match]['address'],
            deadline=config['deadline'],
            price_limit=config['price_limit'],
        )

        message = '\r\n'.join([
            "From: {sender}",
            "To: {to}",
            "Subject: {subject}",
            "",
            "{body}"
        ]).format(
            sender=config['email']['user'],
            subject='Your Secret Santa Match',
            to=config['participants'][person]['email'],
            body=body
        )

        try:
            server = smtplib.SMTP(config['email']['server'])
            server.ehlo()
            server.starttls()
            server.login(config['email']['user'], config['email']['pass'])
            server.sendmail(
                config['email']['user'],
                config['participants'][person]['email'],
                message
            )
        finally:
            server.close()


def main():
    config = read_config()
    names = config['participants'].keys()

    shakes = 0
    while True:
        try:
            shakes += 1
            random.shuffle(names)
            pairs = get_pairs(names, config['participants'])
            break
        except ShakeAgain:
            if shakes > 100:
                print('Already shook 100 times.  Something is probably broken')
                exit(1)

    from pprint import pprint
    print('Done pairing.  Took {} shake(s)'.format(shakes))
    send_emails(pairs, config)


if __name__ == '__main__':
    main()
