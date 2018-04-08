import csv
import json

from copy import deepcopy

INPUT_CSV = "output.csv"
OUT_JSON = "output.json"

data = {"name": "JOBS", "children": []}
leaf = {"name": "", "children": []}


def assignment(node, a, current, total):
    if (total == 0):
        return (1)
    temp = deepcopy(leaf)
    if (current == 3):
        temp["name"] = "Total no of jobs: " + a[current];
        temp["size"] = 3938

    else:
        temp["name"] = a[current].strip();
        temp["children"] = []

    node.append(temp)
    temp2 = temp["children"]
    current = current + 1;
    total = total - 1;
    assignment(temp2, a, current, total)


def initialise(a):
    if (data["children"] == []):
        assignment(data["children"], a, 0, 4)
    else:
        for i in data["children"]:
            i_children = [i['name'] for i in data["children"]]
            if a[0] not in i_children:  # loc
                assignment(data["children"], a, 0, 4)  # locations
                return (1)
            else:
                index = i_children.index(a[0].strip())
                for j in data["children"][index]["children"]:
                    j_children = [j['name'] for j in data["children"][index]["children"]]
                    if a[1].strip() not in j_children:  # industry
                        assignment(data["children"][index]["children"], a, 1, 3)
                        return (1)
                    else:
                        index2 = j_children.index(a[1].strip())
                        for k in data["children"][index]["children"][index2]["children"]:  # skil\
                            k_children = [k['name'] for k in data["children"][index]["children"][index2]["children"]]
                            if a[2] not in k_children:
                                assignment(data["children"][index]["children"][index2]["children"], a, 2, 2)  # skills
                                return (1)
                            else:
                                index3 = k_children.index(a[2].strip())
                                for l in data["children"][index]["children"][index2]["children"][index3]["children"]:
                                    temp = l["name"].split(": ")[1]
                                    l["name"] = "Total no of jobs: " + str(int(temp) + int(a[3]));
                                    return (1)


with open(INPUT_CSV) as csvfile:
    a = csv.DictReader(csvfile)

    all_rows = []
    for row in a:
        all_rows.append([row["joblocation_address"], row["industry"], row["skills"], row["\tnumberofpositions"]])

    for a in all_rows:
        initialise(a)

    # print(json.dumps(data, indent=4))
    with open(OUT_JSON, "w") as output_f:
        json.dump(data, output_f)
