
import std.stdio;
import std.file;
import std.conv;
import std.array;
import std.algorithm;

struct Ingredient {
	string name;
	int capacity;
	int durability;
	int flavor;
	int texture;
}

int main()
{
	auto ingredients = readIngredients("input.txt");
	int maxScore = findMaxScore(ingredients, 100);
	writeln(maxScore);
	return 0;
}

Ingredient[] readIngredients(string filename)
{
	auto file = File(filename, "r");
	string line;
	Ingredient[] ingredients;

	while ((line = file.readln()).length > 0)
	{
		auto parts = line.split;
		if (parts.length < 11)
			continue;

		int capacity = to!int(parts[2][0 .. $-1]);
		int durability = to!int(parts[4][0 .. $-1]);
		int flavor = to!int(parts[6][0 .. $-1]);
		int texture = to!int(parts[8][0 .. $-1]);

		ingredients ~= Ingredient(parts[0], capacity, durability, flavor, texture);
	}

	return ingredients;
}

int findMaxScore(Ingredient[] ingredients, int totalTeaspoons)
{
	return calculateMaxScore(ingredients, 0, totalTeaspoons, []);
}

int calculateMaxScore(Ingredient[] ingredients, int index, int remaining, int[] teaspoons)
{
	if (index == ingredients.length - 1)
	{
		teaspoons ~= remaining;
		return score(ingredients, teaspoons);
	}

	int maxScore = 0;
	foreach (i; 0 .. remaining + 1)
	{
		auto score = calculateMaxScore(ingredients, index + 1, remaining - i, teaspoons ~ i);
		if (score > maxScore)
			maxScore = score;
	}
	return maxScore;
}

int score(Ingredient[] ingredients, int[] teaspoons)
{
	int capacity = 0;
	int durability = 0;
	int flavor = 0;
	int texture = 0;

	foreach (i, ingredient; ingredients)
	{
		capacity += ingredient.capacity * teaspoons[i];
		durability += ingredient.durability * teaspoons[i];
		flavor += ingredient.flavor * teaspoons[i];
		texture += ingredient.texture * teaspoons[i];
	}

	if (capacity < 0) capacity = 0;
	if (durability < 0) durability = 0;
	if (flavor < 0) flavor = 0;
	if (texture < 0) texture = 0;

	return capacity * durability * flavor * texture;
}
