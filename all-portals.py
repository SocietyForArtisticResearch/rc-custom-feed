import json

with open('/var/www/html/internal_research.json', 'r') as file:
    data = json.load(file)


portals = {}

for entry in data:
    for field in ('published_in', 'connected_to'):
        items = entry.get(field, [])

        if isinstance(items, dict):
            items = [items]

        for item in items:
            if 'name' in item:
                portals[item['name']] = item['id']

with open('all_portals.json', 'w') as outfile:
    json.dump(portals, outfile)

print(len(portals)) #42
