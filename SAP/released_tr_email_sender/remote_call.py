import os
import json
from pyrfc import Connection
from dotenv import load_dotenv, find_dotenv


def z_get_released_transports(age_in_days):
    env_file = find_dotenv(usecwd=True)
    load_dotenv(dotenv_path=env_file)

    connection_attributes = {
        'user'      : os.getenv('USER'),
        'passwd'    : os.getenv('PASSWORD'),
        'ashost'    : os.getenv('ASHOST'),
        'sysnr'     : os.getenv('SYSNR'),
        'client'    : os.getenv('CLIENT'),
    }

    if None in connection_attributes.values():
        return None

    with Connection(**connection_attributes) as connection:
        json_string = connection.call(
                "Z_GET_RELEASED_TRANSPORTS",
                AGE_IN_DAYS=age_in_days)['JSON_DATA']

    parsed_data = json.loads(json_string)
    return parsed_data
