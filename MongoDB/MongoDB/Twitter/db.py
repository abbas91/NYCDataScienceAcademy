import pymongo


def save_to_mongo(data, mongo_db, mongo_db_coll):
    # Connects to the MongoDB server running on
    # localhost:27017 by default

    client = pymongo.MongoClient()

    # Get a reference to a particular database

    db = client[mongo_db]

    # Reference a particular collection in the database

    coll = db[mongo_db_coll]

    # Perform a bulk insert and  return the IDs

    return coll.insert(data)
