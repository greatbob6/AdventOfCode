<Query Kind="Program" />

void Main()
{
	var initialPositionData = GetData("puzzle");
	
	int[][] positionData = new int[initialPositionData.Length * 5][];
	InitializeFinalArray(ref positionData, ref initialPositionData);

	var maxX = positionData[0].Length - 1;
	var maxY = positionData.Length - 1;

	var destination = new Point(maxX, maxY);

	var result = A_Star(new Point(0, 0), destination, maxX, maxY, positionData);
	var cost = CalculateCost(result, positionData);
	
	cost.Dump();
	result.Dump();
}

record Point(int X, int Y);

int CalculateCost(List<Point> points, int[][] positionData)
{
	int cost = 0;
	
	foreach (var p in points)
	{
		cost += positionData[p.Y][p.X];
	}
	
	return cost - positionData[0][0];
}

int[][] GetData(string prefix)
{
	var path = Path.Combine(Path.GetDirectoryName(Util.CurrentQueryPath), $"{prefix}_input.txt");

	var lines = File.ReadAllLines(path);

	var array = lines
		.Select(l => l.ToCharArray())
		.Select(ca => ca.Select(c => Convert.ToInt32(c.ToString())).ToArray())
		.ToArray();
	
	return array;
}

bool IsInBounds(int px, int py, int maxX, int maxY)
{
	return (px >= 0 && px <= maxX) && (py >= 0 && py <= maxY);
}

Point[] GetNeighbors(int x, int y, int maxX, int maxY)
{
	var neighbors = new Point[] { new Point(x, y - 1), new Point(x + 1, y), new Point(x, y + 1), new Point(x - 1, y) };

	return neighbors
		.Where(p => IsInBounds(p.X, p.Y, maxX, maxY))
		.ToArray();
}

int GetGScore(Dictionary<Point, int> gScores, Point current)
{
	if (gScores.ContainsKey(current))
	{
		return gScores[current];
	}
	else
	{
		return 2000000000;
	}
}

int DistanceToDestination(int cx, int cy, int maxX, int maxY)
{
	return (maxX - cx) + (maxY - cy);
}

int d(int nx, int ny, int[][] positionData)
{
	return positionData[ny][nx];
}

List<Point> ReconstructPath(Dictionary<Point, Point> cameFrom, Point current)
{
	List<Point> totalPath = new List<Point> { current };
	
	while (cameFrom.ContainsKey(current))
	{
		current = cameFrom[current];

		totalPath.Insert(0, current);
	}
        
	return totalPath;
}

// A* finds a path from start to goal.
// h is the heuristic function. h(n) estimates the cost to reach goal from node n.
List<Point> A_Star(Point start, Point goal, int maxX, int maxY, int[][] positionData)
{
	// The set of discovered nodes that may need to be (re-)expanded.
	// Initially, only the start node is known.
	// This is usually implemented as a min-heap or priority queue rather than a hash-set.
	var openSet = new PriorityQueue<Point, int>();
	openSet.Enqueue(start, 0);

	// For node n, cameFrom[n] is the node immediately preceding it on the cheapest path from start
	// to n currently known.
	var cameFrom = new Dictionary<Point, Point>();

	// For node n, gScore[n] is the cost of the cheapest path from start to n currently known.
	var gScore = new Dictionary<Point, int>()
	{
		{ start, 0 }
	};

	// For node n, fScore[n] := gScore[n] + h(n). fScore[n] represents our current best guess as to
	// how short a path from start to finish can be if it goes through n.
	var fScore = new Dictionary<Point, int>()
	{
		{ start, DistanceToDestination(start.X, start.Y, goal.X, goal.Y) }
	};

	while (openSet.Count > 0)
	{
		// This operation can occur in O(1) time if openSet is a min-heap or a priority queue
		var current = openSet.Dequeue();

		if (current == goal)
		{
			return ReconstructPath(cameFrom, current);
		}

		var neighbors = GetNeighbors(current.X, current.Y, maxX, maxY);
		foreach (var neighbor in neighbors)
		{
			// d(current,neighbor) is the weight of the edge from current to neighbor
			// tentative_gScore is the distance from start to the neighbor through current
			var tentative_gScore = GetGScore(gScore, current) + d(neighbor.X, neighbor.Y, positionData);

			if (tentative_gScore < GetGScore(gScore, neighbor))
			{
				// This path to neighbor is better than any previous one. Record it!
				cameFrom[neighbor] = current;
				gScore[neighbor] = tentative_gScore;
				fScore[neighbor] = tentative_gScore + DistanceToDestination(neighbor.X, neighbor.Y, maxX, maxY);

				if (!openSet.UnorderedItems.Any(i => i.Element == neighbor))
				{
					openSet.Enqueue(neighbor, fScore[neighbor]);
				}
			}
		}
	}

	// Open set is empty but goal was never reached
	throw new Exception("No goal found");
}

void InitializeFinalArray(ref int[][] final, ref int[][] initialArray)
{
	for (int i = 0; i < initialArray[0].Length * 5; i++)
	{
		final[i] = new int[initialArray[0].Length * 5];
	}

	var newArray = initialArray;
	CopyToFinal(ref final, initialArray, 0, 0);

	for (int x = 1; x < 5; x++)
	{
		newArray = DuplicateArrayAndAddOne(newArray);
		CopyToFinal(ref final, newArray, x, 0);
	}

	for (int i = 0; i < 4; i++)
	{
		for (int j = 0; j < initialArray.Length; j++)
		{
			CopyRow(ref final, i * initialArray.Length + j, (i + 1) * initialArray.Length + j);
		}
	}
}

int[][] DuplicateArrayAndAddOne(int[][] input)
{
	return input.Select(a => a.Select(i => i == 9 ? 1 : i + 1).ToArray()).ToArray();
}

void CopyToFinal(ref int[][] final, int[][] input, int subSectionX, int subSectionY)
{
	int baseX = subSectionX * input[0].Length;
	int baseY = subSectionY * input.Length;

	for (var y = 0; y < input.Length; y++)
	{
		for (var x = 0; x < input[0].Length; x++)
		{
			final[baseY + y][baseX + x] = input[y][x];
		}
	}
}

void CopyRow(ref int[][] final, int sourceRow, int destRow)
{
	for (int x = 0; x < final[0].Length; x++)
	{
		int val = final[sourceRow][x];

		final[destRow][x] = val == 9 ? 1 : val + 1;
	}
}
