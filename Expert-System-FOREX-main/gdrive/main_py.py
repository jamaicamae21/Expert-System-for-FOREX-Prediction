from swiplserver import PrologMQI, PrologThread
import MetaTrader5 as mt5  # pip install MetaTrader5
import pandas as pd  # pip install pandas
import plotly.express as px  # pip install plotly
import json
import pprint
from datetime import datetime, timedelta

# credentials
path = 'C:\\Program Files\\\MetaTrader 5\\terminal64.exe'
account = "XXXX"  # change with your credentials
password = 'XXXXX'  # change with your credentials
server = 'XXXXX'  # change with your credentials


def get_info(symbol):
    """https://www.mql5.com/en/docs/integration/python_metatrader5/mt5symbolinfo_py"""
    # get symbol properties
    info = mt5.symbol_info(symbol)
    return info


def open_trade(action):
    # prepare the buy request structure
    symbol = 'XXXX' # Change the XXXX with your currency pair

    if action == 'buy':
        trade_type = mt5.ORDER_TYPE_BUY
        price = mt5.symbol_info_tick(symbol).ask
    elif action == 'sell':
        trade_type = mt5.ORDER_TYPE_SELL
        price = mt5.symbol_info_tick(symbol).bid
    point = mt5.symbol_info(symbol).point

    buy_request = {
        "action": mt5.TRADE_ACTION_DEAL,
        "symbol": "XXXX",  # Change the XXXX depends on what currency pair you use,
        "volume": 0.1,  # FLOAT
        "type": trade_type,
        "price": price,
        "sl": 0.0,  # FLOAT
        "tp": 0.0,  # FLOAT
        "deviation": 20,  # INTERGER
        "magic": 234000,  # INTERGER
        "comment": "python script open",
        "type_time": mt5.ORDER_TIME_GTC,
        "type_filling": mt5.ORDER_FILLING_IOC,
    }
    # send a trading request
    result = mt5.order_send(buy_request)
    return result, buy_request


def close_trade(ticket, action):
    """https://www.mql5.com/en/docs/integration/python_metatrader5/mt5ordersend_py"""

    # create a close request
    symbol = 'XXXX' # Change the XXXX with your currency pair
    if action == 'buy':
        trade_type = mt5.ORDER_TYPE_BUY
        price = mt5.symbol_info_tick(symbol).ask
    elif action == 'sell':
        trade_type = mt5.ORDER_TYPE_SELL
        price = mt5.symbol_info_tick(symbol).bid

    close_request = {
        "action": mt5.TRADE_ACTION_DEAL,
        "symbol": "XXXX",   # Change the XXXX depends on what currency pair you use,
        "volume": 0.1,
        "type": trade_type,
        "position": ticket,
        "price": price,
        "deviation": 20,
        "magic": 234000,
        "comment": "python script close",
        "type_time": mt5.ORDER_TIME_GTC,  # good till cancelled
        "type_filling": mt5.ORDER_FILLING_IOC,
    }
    # send a close request
    result = mt5.order_send(close_request)


# Function to start Meta Trader 5 (MT5)
def start_mt5(username, password, server, path):
    uname = int(username)
    pword = str(password)
    trading_server = str(server)
    filepath = str(path)

    # Attempt to start MT5
    if mt5.initialize(login=uname, password=pword, server=trading_server, path=filepath):
        print("Trading Bot Starting")
        # Login to MT5
        if mt5.login(login=uname, password=pword, server=trading_server):
            print("Trading Bot Logged in and Ready to Trade!")

            account_info = mt5.account_info()
            print(account_info)

            # getting specific account data
            account_name = account_info.name
            login_number = account_info.login
            balance = account_info.balance
            equity = account_info.equity

            print()
            print('name: ', account_name)
            print('login: ', login_number)
            print('balance: ', balance)
            print('equity: ', equity)
            return True
        else:
            print("Login Fail")
            quit()
            return PermissionError
    else:
        print("MT5 Initialization Failed")
        quit()
        return ConnectionAbortedError


def get_candlestick():
    # Request the last 60 candlesticks for EURUSD M1  
    symbol = "XXXX",  # Change the XXXX depends on what currency pair you use
    timeframe = mt5.TIMEFRAME_M1    # 1 minute generate of candle stick
    total_candles = 60
    candlesticks = mt5.copy_rates_from_pos(symbol, timeframe, 0, total_candles)

    # Convert the candlestick data to a pandas DataFrame
    df = pd.DataFrame(candlesticks)

    # Convert the timestamp to a human-readable format
    df['time'] = pd.to_datetime(df['time'], unit='s')

    # Rearrange the columns to match the OHLC format
    ohlc_data_m1 = df[['open', 'high', 'low', 'close']]

    ohlc_data_m1.to_csv('ohlc_data_m1.csv', index=False, header=False)

    return ohlc_data_m1


def expert_system():
    # start of inference
    with PrologMQI() as mqi:
        with mqi.create_thread() as prolog_expert_system:
            # Consult the file
            prolog_expert_system.query("consult('main_es.pl')")

            # Query the main/0 predicate
            prolog_expert_system.query("main.")
            result = prolog_expert_system.query("fact(order(X))")

    expert_result = result[0]['X']
    return expert_result


if __name__ == '__main__':

    start_mt5(account, password, server, path)
    get_candlestick()

    expert_result = expert_system()

    print(expert_result)

    # total number of positions
    num_positions = mt5.positions_total()

    # list of positions
    positions = mt5.positions_get()

    if num_positions == 0:
        print()
    else:
        ticket = positions[0].ticket

    if num_positions == 0:
        if expert_result == "buy":
            open_trade('buy')
        elif expert_result == "sell":
            open_trade('sell')
        elif expert_result == "hold":
            print('Hold order')  # or do nothing

    mt5.shutdown()
