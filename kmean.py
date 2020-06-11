# generate/ import points
import random
import math
def distance(vectorA, vectorB):
    return math.sqrt(sum([ (x1-x2)**2for x1, x2 in zip(vectorA, vectorB) ]))

def kmean(k, points):
    if k <= 0: return
    classifications = [0] * len(points)
    if k == 1: return classifications
    # randomly sample k points and copy position to make starting centroids
    centroids = random.sample(points, k)
    for w in range(100):
        # compute sum of distence between point and centroids
        # assign point to closest centroid
        for i, point in enumerate(points):
            classifications[i] = min(range(k), key=lambda x: distance(point, centroids[x]))
        # new centroid is the average position of all points assigned to it
        for i, centroid in enumerate(centroids):
            group = [ points[j] for j, c in enumerate(classifications) if c == i ]
            centroids[i] = tuple([ sum(a)/len(a) for a in zip(*group) ])
    return classifications
print(kmean(2, [(1,2), (1,3), (0,-2), (-1,-1)]))
