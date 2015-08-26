#import random
import test_run
#from datetime import date, timedelta

from event import OrderEvent


class TestRandomStrategy(object):
    def __init__(self, instrument, units, events):
        self.instrument = instrument
        self.units = units
        self.events = events
        self.ticks = 0

    def calculate_signals(self, event):
#        yesterday = date.today() - timedelta(days=1)
        
        if event.type == 'TICK':
            self.ticks += 1
            if self.ticks % 1 == 0:
                print('in the if')
                side = test_run.run_test() #random.choice(["buy", "sell"])
                order = OrderEvent(
                    self.instrument, self.units, "market", side
                )
                self.events.put(order)

'''
        if event.type == 'DAY':
            if event.date > yesterday :
                side = test_run.run_test() #random.choice(["buy", "sell"])
                order = OrderEvent(
                    self.instrument, self.units, "market", side
                )
                self.events.put(order)
'''