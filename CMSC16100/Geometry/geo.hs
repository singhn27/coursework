-- Geometry Lab

-- A point is a point in the xy plane, represented by x and y coordinates
-- E.g. (Point 0.0 0.0) is the origin, (Point (-1) (1)) is in the top left
-- quadrant.
data Point = Point Double Double
    deriving (Show, Eq)

-- A line segment is a straight line of finite length, defined by its
-- two end points.   E.g. (LineSegment (Point 0 0) (Point 1 1)) is a
-- line segment from the origin to the coordinate (1, 1)
data LineSegment = LineSegment Point Point 
    deriving (Show, Eq)


-- A Path is a 2D path in the xy-plane.  The idea is that Path can be 
-- extended to support straight lines, curves, and arbitrary paths, 
-- but currently there is only one data constructor for Path: Line.
data Path = 
-- Line represents an infinite straight line defined by its slope a
-- and its y intercept b, ie. by the equation y = ax + b
    Line Double Double
    deriving (Show, Eq)

intersects :: Path -> LineSegment -> Bool
intersects (Line a b) (LineSegment (Point x3 y3) (Point x4 y4)) = result where
    result = if denominator == 0 then False else boundsCheck
    (x1, y1) = (0, b)
    (x2, y2) = (1, a+b)
    denominator   = (x1 - x2)*(y3 - y4) - (y1 - y2)*(x3 - x4)
    intersectionx = ((x1*y2 - y1*x2)*(x3-x4) - (x1 - x2)*(x3*y4 - y3*x4))/denominator
    intersectiony = ((x1*y2 - y1*x2)*(y3-y4) - (y1 - y2)*(x3*y4 - y3*x4))/denominator
    boundsCheck   = liesInSegment (Point x3 y3) (Point x4 y4) (Point intersectionx intersectiony)

pointFallsInInterval :: Double -> Double -> Double -> Bool
pointFallsInInterval x1 x2 myX | x1 <= x2   = myX >= x1 && myX <= x2
                               | otherwise = myX <= x1 && myX >= x2

liesInSegment :: Point -> Point -> Point -> Bool
liesInSegment (Point x1 y1) (Point x2 y2) (Point myX myY) = result where
    result = (pointFallsInInterval x1 x2 myX) && (pointFallsInInterval y1 y2 myY)
    
-------------------------------------------------------------

type Radius = Double

data Shape = Quadrilateral Point Point Point Point 
           | Triangle Point Point Point
           | Circle Point Radius
           | Composite Shape Shape
    deriving (Show, Eq)

data BoundingBox = BoundingBox Point Point
    deriving (Show, Eq)
    
boundShape :: Shape -> BoundingBox
boundShape (Quadrilateral (Point x1 y1) (Point x2 y2) (Point x3 y3) (Point x4 y4)) = result where
    minx = minimum [x1, x2, x3, x4]
    miny = minimum [y1, y2, y3, y4]
    maxx = maximum [x1, x2, x3, x4]
    maxy = maximum [y1, y2, y3, y4]
    result = BoundingBox (Point minx miny) (Point maxx maxy)

boundShape (Triangle (Point x1 y1) (Point x2 y2) (Point x3 y3)) = result where
    minx = minimum [x1, x2, x3]
    miny = minimum [y1, y2, y3]
    maxx = maximum [x1, x2, x3]
    maxy = maximum [y1, y2, y3]
    result = BoundingBox (Point minx miny) (Point maxx maxy)

boundShape (Circle (Point x1 y1) radius) = result where
    result = BoundingBox (Point (x1-radius) (y1-radius)) (Point (x1+radius) (y1+radius))

boundShape (Composite shape1 shape2) = result where
    BoundingBox (Point x1 y1) (Point x2 y2) = boundShape shape1
    BoundingBox (Point x3 y3) (Point x4 y4) = boundShape shape2
    globalminx = min x1 x3
    globalminy = min y1 y3
    globalmaxx = max x2 x4
    globalmaxy = max y2 y4
    result = BoundingBox (Point globalminx globalminy) (Point globalmaxx globalmaxy)
    
----------------------------------------------------------------

boxToLines :: BoundingBox -> [LineSegment]
boxToLines (BoundingBox (Point minx miny) (Point maxx maxy)) = result where
    topleft = (Point minx maxy)
    bottomright = (Point maxx miny)
    bottomleft = (Point minx miny)
    topright = (Point maxx maxy)
    result = [ LineSegment bottomleft topleft
             , LineSegment topleft topright
             , LineSegment topright bottomright
             , LineSegment bottomright bottomleft
             ]

intersectsBB :: Path -> BoundingBox -> Bool
intersectsBB myPath myBox = any (intersects myPath) (boxToLines myBox)      

mightIntersectShape :: Path -> Shape -> Bool
mightIntersectShape myPath myShape = intersectsBB myPath (boundShape myShape)
