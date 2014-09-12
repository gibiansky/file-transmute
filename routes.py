from flask import Flask, render_template
app = Flask(__name__)


@app.route("/")
def index():
    return render_template('index.html')

@app.route("/convert/", methods=["POST"])
def convert():
    return
    # from, to, file (blob of stuff)

if __name__ == "__main__":
    app.run(port=80, host="0.0.0.0")
