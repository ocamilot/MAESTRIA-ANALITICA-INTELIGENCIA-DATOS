from flask import Flask, request
from flask_restx import Api, Resource, fields, reqparse
import joblib
from m09_model_deployment import predict_proba
from flask_cors import CORS
from werkzeug.datastructures import FileStorage
import pandas as pd

app = Flask(__name__)
api = Api(
    app, 
    version='1.0', 
    title='Car Price Prediction API',
    description='Car Price Prediction API'
)

ns = api.namespace('predict', description='Car Price')

# Definición de campos de respuesta
resource_fields = api.model('Resource', {
    'Prediccion_Precios_Carros': fields.List(fields.Float),
})

# Definición de argumentos para la API
parser = reqparse.RequestParser()
parser.add_argument('Year', type=int, required=True, help='Vehicle Year')
parser.add_argument('Mileage', type=int, required=True, help='Vehicle Mileage')
parser.add_argument('State', type=str, required=True, help='State of the vehicle')
parser.add_argument('Make', type=str, required=True, help='Make of the vehicle')
parser.add_argument('Model', type=str, required=True, help='Model of the vehicle')

# Definición de la clase para la API
@ns.route('/')
class CarPriceApi(Resource):

    @api.doc(parser=parser)
    @api.marshal_with(resource_fields)
    def post(self):
        args = parser.parse_args()

        # Llama a la función predict_price para realizar la predicción
        prediction = predict_proba(args['Year'], args['Mileage'], args['State'], args['Make'], args['Model'])

        # Devuelve el resultado como respuesta JSON
        return {'Prediccion_Precios_Carros': [prediction]}, 200

# Inicia el servidor Flask
if __name__ == '__main__':
    app.run(debug=True, use_reloader=False, host='0.0.0.0', port=5000)


