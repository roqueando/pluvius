from typing import Union

from fastapi import FastAPI

from pipeline import run

app = FastAPI()

def check_raining(rain):
    if rain <= 0.5:
        return 'ta seco'
    if rain <= 1.5:
        return 'vai chover um cadin'
    if rain >= 2.0:
        return "AAAAAAAAAAAAI VEM AGUA"
    return "ta seco"

@app.get("/rain")
def will_rain():
    output = run()
    return {'rain': check_raining(output[0])}
