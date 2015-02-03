% Monte Carlo with simulated annealing for TSP optimization
%
% Input: a list of N vertices (2-dimensional coordinates)
% 
% Do: find the shortest possible path through all N vertices
%
% Output: ordered list of the N indices of the verticies that produces the shortest
% path and path length.

% Write a function that takes in the list of coordinates and the
% list of the indices and outputs the total length of the path. This will
% be the objective function. Use the following list of 5 "city" coordinates to
% test your objective function for different order of vertices.
%
% Implementation of a Nearest Neighbor algorithm for the Traveling Salesman Problem

coord = [0 1 -1 -2 2; 0 -1 1 2 -2];
 
coord = coord'
N_cities = size(coord,1);

distances = pdist(coord);
distances = squareform(distances);
distances(distances==0) = realmax;

shortestPathLength = realmax;

for i = 1:N_cities
    
    startCity = i;

    path = startCity;
    
    distanceTraveled = 0;
    distancesNew = distances;
    
    currentCity = startCity; 
    
    for j = 1:N_cities-1
        
        [minDist,nextCity] = min(distancesNew(:,currentCity));
        if (length(nextCity) > 1)
            nextCity = nextCity(1);
        end
        
        path(end+1,1) = nextCity;
        distanceTraveled = distanceTraveled +...
                    distances(currentCity,nextCity);
        
        distancesNew(currentCity,:) = realmax;
        
        currentCity = nextCity;

    end
    
    path(end+1,1) = startCity;
    distanceTraveled = distanceTraveled +...
        distances(currentCity,startCity);
    
    if (distanceTraveled < shortestPathLength)

        shortestPathLength = distanceTraveled;
        shortestPath = path; 

    end 
    
end

% Write a Monte Carlo simulation that uses the objective function
% from task 1.1, and uses the following proposal distribution: take the
% current ordered list of vertices and swap two randomly selected ones
% (this can be done in one line). The proposal candidate will be accepted
% or rejected according to the Metropolis criterion with temperature
% parameter T. Run the simulation for different values of T and oberve the
% effect on convergence. Use the test coordinates provided above, and then
% run your simulation on a larger data set that you load from the file
% Djibouti_cities.mat (which contains coordinates for 38 cities in the
% country of Djibouti). You can find the optimal solution for this dataset.

coord = coord'
N_cities = size(coord,1);

distances = pdist(coord);
distances = squareform(distances);
distances(distances==0) = realmax;

shortestPathLength = realmax;

for i = 1:N_cities
    
    startCity = i;

    path = startCity;
    
    distanceTraveled = 0;
    distancesNew = distances;
    
    currentCity = startCity; 
    
    for j = 1:N_cities-1
        
        [minDist,nextCity] = min(distancesNew(:,currentCity));
        if (length(nextCity) > 1)

            nextCity = nextCity(1);

        end
        
        path(end+1,1) = nextCity;
        distanceTraveled = distanceTraveled +...
                    distances(currentCity,nextCity);
        
        distancesNew(currentCity,:) = realmax;
        
        currentCity = nextCity;
        
    end
    
    path(end+1,1) = startCity;
    distanceTraveled = distanceTraveled +...
        distances(currentCity,startCity);
    
    if (distanceTraveled < shortestPathLength)

        shortestPathLength = distanceTraveled;

        shortestPath = path;

    end 
    
end

for i = 1:5

    i = 1
    randr = round(rand(1,39)*39)
    rand1 = randr(1,1)
    rand2 = randr(1,2)
    shortestPath([rand2,rand1],:) = shortestPath([rand1,rand2],:)
    [val idx] = sort(shortestPath)
    coord = coord(idx,:)
    k = pdist(coord)

end
