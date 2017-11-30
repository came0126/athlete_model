# athlete_model

SML Project Summary - modeling top athletes:

I choose to model top level athletes by calculating a “power index” for each kind of athlete. Instead of only calculating top athletes, I also modeled average college athletes. A power index is a number ranging from 0 to 600. Typically the top modeled athletes will have a power index ranging from 400-500, with a few outliers like Michael Jordan. Average athletes will only have an index range of 150-300 (not implemented yet). I modeled a variety of athletics, which included: baseball players, basketball players, wide receivers, quarterbacks, football centers, swimmers, and powerlifters. I also created lists of average athletes (not implemented yet), top athletes, and all athletes- then added functions to sort the athletes by power index.

Each type of athlete has a corresponding datatype that captures stats of the sport that is important in calculating a power index. For example, wide receivers must be fast, and have an amazing vertical jump. The power index function takes this information and calculates a number. I chose to leave out championship wins, or games won, so anyone could be modeled using this calculator. This power index information is used to sort the lists of athletes, so it is easy to see which athlete has the most athletic ability.

This calculation system has a few problems. First, some types of athletes, like swimmers and quarterbacks, need a fast reaction time or the ability to handle pressure well, these stats are usually impossible to obtain. Second, the power index system is used to rank the ability of each athlete- not necessarily the skill of each athlete. The skill of each athlete would be impossible to calculate without taking championships won into account. Realistically, skill and ability would be used to calculate a ranking in each sport.