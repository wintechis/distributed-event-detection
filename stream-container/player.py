from tkinter import W
import traci
import time
import requests
import csv
import datetime

step = 0

vehicles = {
    "veh11": 8080,
#    "veh21": 8082,
#    "veh31": 8084,
#    "veh41": 8086
}

#template_car = """
#@prefix sosa: <http://www.w3.org/ns/sosa/> .
#@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
#
#<> a sosa:Observation ;
#    sosa:resultTime "{t}Z"^^xsd:dateTimeStamp ;
#    sosa:hasSimpleResult {value} ;
#    sosa:hasFeatureOfInterest <http://example.org/vehicles/{vehicle}> ;
#    sosa:observedProperty <http://example.org/properties/{property}> .
#"""
template_car = """
@prefix sosa: <http://www.w3.org/ns/sosa/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

<>  rdf:subject <http://example.org/vehicles/{vehicle}> ;
    rdf:predicate <http://example.org/properties/{property}> .
    rdf:object {value} ;
    rdf:value {t} .
"""

def post(port, t, vehicle, value, property):
    requests.post("http://localhost:" + str(port) + "/", data=template_car.format(t=time_round(t, datetime.timedelta(seconds=1)).isoformat(), vehicle=vehicle, value=str(value).lower(), property=property))

def time_mod(time, delta, epoch=None):
    if epoch is None:
        epoch = datetime.datetime(1970, 1, 1, tzinfo=time.tzinfo)
    return (time - epoch) % delta

def time_round(time, delta, epoch=None):
    mod = time_mod(time, delta, epoch)
    if mod < delta / 2:
       return time - mod
    return time + (delta - mod)

while True:
    sumoCmd = ["sumo", "-S", "-Q", "-c", "/run/media/daniel/bbcff4ad-55ed-4b85-b467-265ac71ad6af/home/daniel/Git/stream-reasoning-challenge/stream-log-files/sumo/daniel.sumocfg" ]
    traci.start(sumoCmd)

    with open('car.csv', 'w') as dump_file:
        dump = csv.writer(dump_file) 
        dump.writerow(['step', 'time', 'car', 'speed', 'positionX', 'positionY', 'acceleration', 'angle', 'road', 'lane', 'blinkerRight', 'blinkerLeft', 'brake'])

        while traci.simulation.getMinExpectedNumber() > 0:
            curr_time = datetime.datetime.utcnow()
            traci.simulationStep()
            step = step + 1
            print("Step " + str(step) + " " + str(time_round(curr_time, datetime.timedelta(seconds=1))))

            for vehicle, port in vehicles.items():
                if not vehicle in traci.vehicle.getIDList():
                    continue

                speed = round(traci.vehicle.getSpeed(vehicle), 2)
                #post(port, curr_time, vehicle, speed, "speed")

                pos = traci.vehicle.getPosition(vehicle)
                posX = round(pos[0], 2)
                #post(port + 1, template_car.format(step=step, vehicle=vehicle, value=posX, property="positionX"))

                posY = round(pos[1], 2)
                #post(port + 2, template_car.format(step=step, vehicle=vehicle, value=posY, property="positionY"))

                type = traci.vehicle.getTypeID(vehicle)

                acceleration = round(traci.vehicle.getAcceleration(vehicle), 2)
                #post(port + 3, template_car.format(step=step, vehicle=vehicle, value=acceleration, property="acceleration"))

                angle = round(traci.vehicle.getAngle(vehicle), 2)
                #post(port + 4, template_car.format(step=step, vehicle=vehicle, value=angle, property="angle"))

                road = traci.vehicle.getRoadID(vehicle)
                #post(port + 5, template_car.format(step=step, vehicle=vehicle, value=road, property="road"))

                lane = traci.vehicle.getLaneIndex(vehicle)
                #post(port + 6, template_car.format(step=step, vehicle=vehicle, value=lane, property="lane"))

                signals = traci.vehicle.getSignals(vehicle)
                blinkerRight = signals & 0b1 > 0
                #post(port + 7, template_car.format(step=step, vehicle=vehicle, value=blinkerRight, property="blinkerRight"))
                post(port, curr_time, vehicle, blinkerRight, "blinkerRight")

                blinkerLeft = signals & 0b10 > 0
                #post(port + 8, template_car.format(step=step, vehicle=vehicle, value=blinkerLeft, property="blinkerLeft"))

                brake = signals & 0b1000 > 0
                #post(port + 9, template_car.format(step=step, vehicle=vehicle, value=brake, property="brake"))

                dump.writerow([step, curr_time, vehicle, speed, posX, posY, acceleration, angle, road, lane, blinkerRight, blinkerLeft, brake])

            time.sleep(1. - (datetime.datetime.now() - curr_time).microseconds / 1000000)