import requests
import simplejson as json

# let's shorten an url via python
# payload data that you send to url
url = "https://www.googleapis.com/urlshortener/v1/url"

payload = {"longUrl": "http://example.com"}

headers = {"Content-Type": "application/json"}

r = requests.post(url, json=payload, headers=headers)

print(r.headers)


'''
# posting data into url
my_data = {"name": "Nora", "email": "nora@example.co"}
r = requests.post("http://www.w3schools.com/php/welcome.php", data = my_data)

f = open("data.html", "w+")
f.write(r.text)


from io import BytesIO
from PIL import Image

# getting images from the web
r = requests.get("https://purepng.com/public/uploads/large/purepng.com-mariomariofictional-charactervideo-gamefranchisenintendodesigner-1701528634653vywuz.png")
print("Status:", r.status_code)

image = Image.open(BytesIO(r.content))

path = "./image." + image.format

print(image.size, image.format, image.format)

try:
    image.save(path, image.format)
except IOError:
    print("Cannot open image.")


# getting html file from the web
params = {"q": "pizza"}
r = requests.get("http://bing.com/search", params=params)
print("Status:", r.status_code)
print(r.url)

f = open("./page.html", "w+")
f.write(r.text)

'''