#!/usr/bin/env python
#
# Copyright 2008-2010 by Brian Dominy <brian@oddchange.com>
#
# PYLDecider is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
# 
# PYLDecider is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with PYLDecider; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
#

#
# This script simulates games of "Press Your Luck" in order to determine
# strategy.  The basic question it tries to solve is: given some situation
# in the middle of a game (in round 2) in which a player decision is
# required, what is the best decision for that player to make to maximize
# his chances of winning the game?
#
# The program works by following all possible paths from the initial state,
# computing best plays in each substate.  There are actually two types
# of game states: decision states and spinning states.  When deciding whether
# to play or pass, a decision is required.  In these states, if we know the
# win probability for each of the choices, we assume that the player up
# will always make the best decision.  The win probability is "bubbled up"
# from the optimal substate back to the previous state.
#
# The game also involves randomness on the game board which must be
# accounted for.  When a player is spinning, we compute all possible
# outcomes from that spin, and their winning probabilities for the player.
# Then the overall win probability is a weighted sum of the substate
# probabilities.
#
# The simulation enforces all game rules correctly as far as when spins
# are allowed to be passed, and the order of play.  Ties are not considered
# for the sake of simplicity.
#

####################################################################
# CONSTANTS
####################################################################

# The different types of states in the game tree:
#    DECIDING_PLAY : requires a decision of play/pass
#    SPINNING : a commitment to spin has been made, and the spin
#       count already reduced to indicate "board in motion"
#    FINISHED : end-of-game condition has been reached
DECIDING_PLAY, SPINNING, FINISHED = range(3)


# The possible options for DECIDING_PLAY:
SPIN, PASS = range(2)


####################################################################
# SIMULATION FUNCTIONS
####################################################################

def nspaces (n):
	result = ""
	for i in range(n):
		result += "   "
	return result

def debug (*args):
	if (1):
		for arg in args:
			print arg,
		print

def choice_string (choice):
	if (choice == SPIN):
		return "spin"
	elif (choice == PASS):
		return "pass"
	else:
		return ""


####################################################################
# PRESS YOUR LUCK GAME IMPLEMENTATION
####################################################################

class Player:
	MAX_WHAMMIES = 4

	def __init__ (self, id):
		"""Initialize a new player object."""
		self.id = self.pos = id
		self.score = 0
		self.earned = 0
		self.passed = 0
		self.whammies = 0

	def copy (self):
		"""Return a modifiable copy of a player object."""
		thecopy = Player (self.id)
		thecopy.score = self.score
		thecopy.earned = self.earned
		thecopy.passed = self.passed
		thecopy.whammies = self.whammies
		return thecopy


	def __str__ (self):
		"""Return a printable version of a player object.

		For the sake of brevity, all zero values except for the score
		are omitted.
		"""
		result = "($" + str(self.score)
		if self.earned:
			result += " E" + str(self.earned)
		if self.passed:
			result += " P" + str(self.passed)
		if self.whammies:
			result += " W" + str(self.whammies)
		result += ")"
		return result


	def total_spins (self):
		"""Return the total number of spins a player has."""
		return self.earned + self.passed

	def in_game (self):
		"""Return true if a player is still in the game."""
		return self.whammies < Player.MAX_WHAMMIES

	def take_spin (self):
		"""Update when a player commits to taking a spin.

		This will decrement passed spins if it is nonzero, else
		it will decrement earned spins.
		"""
		if self.passed > 0:
			self.passed -= 1
		elif self.earned > 0:
			self.earned -= 1
		else:
			raise RuntimeError ('No spin to take')

	def add_earned (self, num):
		self.earned += num

	def add_passed (self, num):
		self.passed += num

	def award_whammy (self):
		"""Apply a whammy to a player.

		The score is zeroed, the whammy count is increased,
		passed spins are converted to earned, and if the maximum
		number of whammies has been reached, all spins are
		taken away.
		"""
		self.score = 0
		self.whammies += 1
		self.earned += self.passed
		self.passed = 0
		if not self.in_game ():
			self.earned = 0



class Result:
	"""The base class for a board space.

	All board spaces must implement these three methods.  A Result object
	can be thought of as an operator that can be applied to a Player."""
	def is_whammy (self):
		return False
	def get_score (self):
		return 0
	def get_spins (self):
		return 0


class CashResult(Result):
	def __init__ (self, value):
		self.value = value
	def get_score (self):
		return self.value
	def get_name (self):
		return "$" + str(self.value)


class WhammyResult(Result):
	def is_whammy (self):
		return True
	def get_name (self):
		return "whammy"


class CashSpinResult(CashResult):
	def __init__ (self, value, spins = 1):
		CashResult.__init__ (self, value)
		self.spins = spins
	def get_spins (self):
		return self.spins
	def get_name (self):
		return "$" + str(self.value) + "+spin"


class PrizeResult(CashResult):
	def __init__ (self):
		pass
	def get_score (self):
		return 2300
	def get_name (self):
		return "PRIZE"


SimpleBoard = [ (0.16, WhammyResult ()),
	(0.70, CashResult (2000)),
	(0.14, CashSpinResult (1000, 1)) ]


class Board:
	"""The description for an entire gameboard.
	
	A Board object can be described in terms of the three fundamentally different
	types of spaces: whammies, cash only, and cash-plus-spin spaces.  Identical
	board spaces are 'merged' during construction for optimization.  Conceptually,
	the Board stores each of the different spaces and the probability that each
	will occur."""
	def __init__ (self):
		self.count = 0
		
		# After the board is fully constructed, the spaces are separated into the
		# 3 types.  For whammies, only the overall probability is kept.  For the other
		# types, these are lists of (probability, value) tuples.
		self.whammy_prob = 0
		self.fixed = []
		self.bonus = []
		self.win_prob_cache = {}


	def dump (self):
		"""Dump a Board object for debugging purposes."""
		print "Count = ", self.count
		print "Whammy prob = ", self.whammy_prob
		print "Fixed:", self.fixed
		print "Bonus:", self.bonus


	def add (self, *results):
		"""Add a new space to the game board."""
		for result in results:
			if result.is_whammy ():
				self.whammy_prob += 1.0
			else:
				count = 1.0
				if result.get_spins () > 0:
					list = self.bonus
				else:
					list = self.fixed
				for e in list:
					if (e[1] == result.get_score ()):
						count = count + e[0]
						list.remove (e)
				element = (count, result.get_score ())
				list.append (element)

			# TODO - no merging of identical values is happening yet
			#elif result.get_spins () > 0:
			#	self.bonus.append ((1.0, result.get_score ()))
			#else:
			#	self.fixed.append ((1.0, result.get_score ()))
			self.count += 1


	def complete (self):
		"""Say that all game board spaces have been added.

		At this point, analysis of the game board can be done to
		compute constant attributes which are needed repeatedly during
		the simulation.
		"""
		self.whammy_prob = float(self.whammy_prob) / self.count

		new_fixed = []
		for fixed in self.fixed:
			new_fixed.append ((fixed[0] / self.count, fixed[1]))
		self.fixed = new_fixed

		new_bonus = []
		for bonus in self.bonus:
			new_bonus.append ((bonus[0] / self.count, bonus[1]))
		self.bonus = new_bonus

		# For now
		#self.whammy_prob = 0.16
		#self.fixed = [ (0.7, 2000) ]
		#self.bonus = [ (0.14, 1000) ]

		# TODO - actually calculate this
		self.total_bonus_prob = 0.14
		self.max_fixed_value = 2500


	def fixed_prob (self, n):
		"""Return the percentage of board spaces that are a fixed value
		(with no additional spin) and have value greater than n.

		This probability is one component of the overall winning probability
		in an endgame.

		"""
		prob = 0.0
		for fixed in self.fixed:
			if n <= fixed[1]:
				prob += fixed[0]
		return prob


	def win_prob (self, lead):
		"""Return the probability of winning with one spin to go.

		'lead' is negative if the spinning player is behind, else positive.

		Because of the nature of the gameboard, this probability does not
		depend on the actual game scores, but only on the difference between
		the player up and the higher opponent.  Specifically, at most 1 extra
		spin can be earned per turn, and dollar values with an extra spin are
		nonnegative.

		There are two components to winning probability: spaces that offer a
		fixed value only, and spaces that offer an extra spin.  The extra spin
		spaces contribute probabilities that form a converging geometric series
		over multiple spins.  The result requires summing this geometric series
		using a recursive call.  The recursion is terminated by recognizing that
		if the player's lead is greater than the maximum fixed dollar value on
		the board, then extra spin spaces don't really change anything.  This
		case uses the formula for the sum of an infinite geometric series to
		provide the sum without a recursive call.
		"""
		prob = self.fixed_prob (-lead)
		if lead > self.max_fixed_value:
			prob /= (1 - self.total_bonus_prob)
		elif lead in self.win_prob_cache:
			prob = self.win_prob_cache[lead]
		else:
			for bonus in self.bonus:
				prob += bonus[0] * self.win_prob (lead + bonus[1])
			self.win_prob_cache[lead] = prob
			print "win_prob: lead=" + str(lead) + " is " + str(prob)
		return prob


	def test_win_prob (self):
		lead = -6000
		while lead <= 6000:
			prob = self.win_prob (lead)
			print "Lead = %d    Win prob = %f" % (lead, prob)
			lead += 1000


####################################################################
# STRATEGY SIMULATION CLASSES
####################################################################

class Situation:
	pass


class FinalSituation(Situation):
	pass


class DecisionSituation(Situation):
	pass


class PrizeTargetSituation(Situation):
	pass


class PassTargetSituation(Situation):
	pass


class SpinningSituation(Situation):
	pass


class WinVector:
	pass


class Game:
	id = 0
	max_depth = 12
	NUM_PLAYERS = 3

	db = {}

	def __init__ (self, board):
		self.player = [Player(0), Player(1), Player(2)]
		self.swap_vector = [ 0, 1, 2 ]
		self.id = Game.id
		self.best_choice = None
		self.board = board
		Game.id += 1

	def initialize (self):
		"""Complete initialization of a game object.

		"""
		if (self.action != SPINNING):
			self.update_turn ()

	def __str__ (self):
		if (self.action == SPINNING):
			string = "Spinning>"
		elif (self.action == DECIDING_PLAY):
			string = "Deciding>"
		elif (self.action == FINISHED):
			string = "Finished>"

		string += " {"
		for player in self.player:
			string += str(player) + " "
		string += "} ID" + str(self.id)

		if self.best_choice == SPIN:
			string += " (should spin)"
		elif self.best_choice == PASS:
			string += " (should pass)"

		if self.action == SPINNING and self.total_spins () == 0:
			string += " (final spin)"

		return string

	def id_string (self):
		"""Return the ID string for this game state.
		"""
		return "ID" + str(self.id)

	def hash_key (self):
		"""Return a hash key for this game state.

		The hash key is the minimal string that identifies whether
		two game state objects are equal.
		"""
		key = str(self.action) + ':'
		for player in self.player:
			key += str(player.score) + '-'
			key += str(player.earned) + '-' + str(player.passed) + '-'
			key += str(player.whammies) + '-'
		return key

	def copy (self):
		"""Return a modifiable copy of this game state.
		"""
		newgame = Game (self.board)
		newgame.player = [self.player[0].copy (),
			self.player[1].copy (),
			self.player[2].copy () ]
		newgame.action = self.action
		return newgame

	def player_up (self):
		"""Return a reference to the active player in a game.
		"""
		return self.player[0]

	def lower_opponent (self):
		"""Return a reference to the active player's trailing opponent.
		"""
		return self.player[1]

	def higher_opponent (self):
		"""Return a reference to the active player's leading opponent.
		"""
		return self.player[2]

	def total_spins (self):
		"""Return the total number of spins for all players.
		"""
		return (self.player[0].total_spins () +
			self.player[1].total_spins () +
			self.player[2].total_spins ())

	def is_terminal (self):
		"""Return true if this is a terminal state; i.e. the game is over.
		"""
		return self.action != SPINNING and self.total_spins () == 0

	def unswap (self, v):
		"""Return a win vector, with its entries permuted based on
		previous player swaps.

		"""
		w = [0, 0, 0]
		for i in range(Game.NUM_PLAYERS):
			w[i] = v[self.swap_vector[i]]
		return w


	def swap_players (self, a, b):
		"""Swap the positions of two players.

		The player list is always kept ordered based on the game situation.
		Specifically, the order is always [ current, lower opponent,
		higher opponent ].  When the order needs to be changed, this
		function moves the players into the proper slots, and also makes
		note that this was done, so that the original order can be restored
		later.
		"""
		self.swap_vector[a], self.swap_vector[b] = self.swap_vector[b], self.swap_vector[a]
		self.player[a], self.player[b] = self.player[b], self.player[a]


	def update_turn (self):
		"""Update after a change that might have changed the current player.

		This function is invoked in two cases: after deciding to pass spins,
		and after playing a spin.  It is not called after deciding to play,
		because the current player does not change in that case.

		The next action required is also updated; if a decision is required,
		then it is set to DECIDING_PLAY.  When there are passed spins to be
		played by the next player, that state is skipped entirely, and is
		set to SPINNING.  In the latter case, the spin count is also
		decremented here for the upcoming spin.

		This function also updates the order of the player objects, based
		on the rules of the game (initial order of play and rules of
		passing).

		"""
		if (self.is_terminal ()):
			self.action = FINISHED

		elif (self.player_up ().passed > 0):
			self.play_spin ()

		elif (self.player_up ().earned > 0):
			self.action = DECIDING_PLAY

		else:
			# Otherwise, determine who the next player up should be.
			# If player[1] has spins, then use him; else player[2].
			if (self.player[1].total_spins () > 0):
				self.swap_players (0, 1)
			else:
				self.swap_players (0, 2)

			# Make sure that player[2] > player[1] always.
			if (self.player[1].score > self.player[2].score):
				self.swap_players (1, 2)

			# Set the action based on the new player up.
			if (self.player_up ().passed > 0):
				self.play_spin ()
			else:
				self.action = DECIDING_PLAY


	###### Operations on a SPINNING game state #########

	def apply (self, result):
		"""Apply a game board result to the current game state.

		"""
		if result.is_whammy ():
			self.player_up().award_whammy ()
		else:
			self.player_up().score += result.get_score ()
			self.player_up().earned += result.get_spins ()
		self.update_turn ()

	###### Operations on a DECIDING_PLAY game state #########

	def play_spin (self):
		"""Modify a decision state by choosing to play.

		"""
		if (self.player_up().total_spins == 0):
			raise RuntimeError ('Cannot play spin')
		self.action = SPINNING
		self.player_up ().take_spin ()

	def pass_spins (self):
		"""Modify a decision state by choosing to pass.

		"""
		if (self.player_up().passed > 0):
			raise RuntimeError ('Cannot pass spins')
		self.higher_opponent().add_passed (self.player_up ().earned)
		self.player_up ().earned = 0
		self.update_turn ()

	############# Simulation-only functions ####################

	def log (self, depth, *args):
		debug (nspaces(depth), self.id_string (), ":", *args)

	def get_pessimistic_win_vector (self):
		if (self.lower_opponent ().total_spins () == 0):
			return [0.5, 0.0, 0.5]
		else:
			return [1.0/3, 1.0/3, 1.0/3]

	def get_win_vector (self, depth = 0):
		"""Return the win vector for a game state, which is the probability
		of winning for each of the players.

		This is the top-level interface to determine optimal strategy.
		If the state has already been calculated and is in the cache, then
		it is returned immediately.  Otherwise it must be calculated and
		saved.

		"""
		# TODO - keep statistics on the helpfulness of caching
		key = self.hash_key ()
		try:
			v = Game.db[key]
			#debug (nspaces(depth), self.id_string (), self, "(cached)", v)
			self.log (depth, self, "(cached)", v)
		except KeyError:
			v = self.compute_win_vector (depth)
		return v


	def save_win_vector (self, v):
		"""Save the state's win vector in cache after calculating it.

		The maximum cache size is to limit memory usage.
		"""
		if (len (Game.db) < 25000):
			key = self.hash_key ()
			Game.db[key] = v


	def obvious_choice (self):
		"""Return a subset of the choices that are feasible for exploring.

		In certain cases, it can be proven that a particular choice is
		clearly good/bad.  Instead of following all paths, the search can
		be restricted to the 'obvious' choices.

		"""

		### Obvious rule #1:
		# If you are in third place, and second place has no spins, you must
		# PLAY.  Passing would send the spins to the leader and completely
		# take you out of the game.
		###
		if ((self.player_up().score < self.lower_opponent ().score) and
			(self.lower_opponent ().total_spins () == 0)):
			return SPIN

		return None


	def compute_win_vector (self, depth = 0):
		"""Compute the win vector for a state by recursively checking each of
		its successor states.  The win vector is a 3-tuple where each element
		indicates the probability of that player winning the game based on
		absolute correct play by all players.  The first element is always for the
		player up.

		"""
		#debug (nspaces(depth), self.id_string (), ":", self, "depth", depth)
		self.log (depth, self, "depth", depth)

		# Initialize the result to all zeroes, meanning that the result is completely
		# unknown.
		v = [0.0, 0.0, 0.0]

		# If this is a final state, then the win vector is absolutely
		# determinable.  Ties are not supported, so exactly one player
		# is marked as the winner.
		if self.action == FINISHED:
			max = 0
			winner = None
			for p in range (Game.NUM_PLAYERS):
				if self.player[p].whammies < Player.MAX_WHAMMIES and self.player[p].score > max:
					max = self.player[p].score
					winner = p
			if winner != None:
				v[winner] = 1.0

		elif self.action == DECIDING_PLAY:
			# Decide which choices to consider: normally, both PASS and SPIN, but
			# possibly only one if it is obvious.  ??? If only one choice, then do we even
			# need to calculate the win vector?
			choice = self.obvious_choice ()
			if choice is not None:
				choices = [ choice ]
			else:
				# TODO - handle pass_spins() when can choose target recipient.
				choices = [ PASS, SPIN ]

			# For each possible choice, generate a new state that makes the choice.
			for choice in choices:
				next_state = self.copy ()
				if choice == SPIN:
					next_state.play_spin ()
				else:
					next_state.pass_spins ()

				debug (nspaces(depth), self.id_string (), "If choice ", choice_string (choice))
				win_vector = next_state.unswap (next_state.get_win_vector (depth+1))
				debug (nspaces(depth), self.id_string (), "Sub-vector ", win_vector)

				# The key to computing a DECIDING_PLAY state: choose the
				# substate that offers the best results for the player up.
				if (win_vector[0] > v[0]):
					v = win_vector
					self.best_choice = choice

		elif self.action == SPINNING:
			if self.total_spins () == 0:
				# This is a final spin.  Replace infinite recursion by an
				# exact calculation using a converging geometric series.
				lead = self.player_up ().score - self.higher_opponent ().score
				win_prob = self.board.win_prob (lead)
				v = [ win_prob, 0, 1.0 - win_prob ]
				debug (nspaces(depth), self.id_string (), "Final spin", v)

			elif depth > Game.max_depth:
				# This is not a final spin, but we have been recursing too
				# deeply.  At such depths, the result for this sub-state will
				# have little effect on the result of the original state
				# (at depth 0).  Therefore, we can provide a rough, pessimistic
				# result here -- that does not involve recursion of its
				# sub-states -- and that will be good enough for the caller.
				# TODO: However, states at depth-1, depth-2, etc. which are
				# close to the maximum depth will not be as accurate either.
				# These state values are getting cached and reused.  So the
				# cache entry needs to reflect that.
				v = self.get_pessimistic_win_vector ()
				#debug (nspaces(depth), self.id_string (), "(pessimistic)")
				self.log (depth, "(pessimistic)")

			else:
				# The ordinary method to use on a spinning state is to generate
				# all possible resulting states (based on all possible outcomes
				# of a random spin), compute the win probability for each of them
				# recursively, then combine all of those sub-results together
				# using a weighted sum.
				i = 0
				for poss in SimpleBoard:  # TODO - not using real game board yet
					state = self.copy ()
					state.apply (poss[1])

					debug (nspaces(depth), self.id_string (), "After ", poss[1].get_name ())
					state_win_vector = state.unswap (state.get_win_vector (depth+1))
					debug (nspaces(depth), self.id_string (), "Sub-vector ", state_win_vector)

					for p in range(Game.NUM_PLAYERS):
						v[p] += state_win_vector[p] * poss[0]

					i = i + 1

				# Only in this case where we did a full exact calculation, do we
				# cache that result for reuse later.
				self.save_win_vector (v)

		debug (nspaces (depth), self.id_string (), "Result:", v, choice_string (self.best_choice))

		# The sum of the win probabilities per player should not exceed 1.00.
		# Allow a little slack since floating-point is imprecise sometimes.
		if (v[0] + v[1] + v[2]) > 1.05:
			raise RuntimeError ('Invalid win vector')
		return v


#	def analyze (self, depth = 0):
#		if (self.action == FINISHED):
#			return self.analyze_final (depth)
#		elif (self.action == DECISION_PLAY):
#			return self.analyze_play_decision (depth)
#		elif (self.action == SPINNING):
#			return self.analyze_spin (depth)
#		else:
#			raise RuntimeError ('Invalid action')
#
#
#	def analyze_final (self, depth = 0):
#		pass
#
#
#	def analyze_play_decision (self, depth = 0):
#		pass
#
#
#	def analyze_spin (self, depth = 0):
#		pass


class Endgame(Game):
	"""A subclass of Game that can more easily be initialized to a final state.

	An EndGame object can be created in place of a Game object when there are
	only two players remaining, with no passed spins and without consideration
	of whammying out."""
	def __init__ (self, score, opp_score, spins = 1):
		Game.__init__ (self)
		self.action = DECIDING_PLAY
		self.player[0].score = score
		self.player[0].earned = spins
		self.player[2].score = opp_score
		self.initialize ()


class EndgameTester:
	"""A test class that runs a large number of Endgame() scenarios."""
	def __init__ (self):
		for score in range (0, 10000, 1000):
			for opp_score in range (0, 10000, 1000):
				for spins in range (1, 5):
					game = Endgame(score, opp_score, spins)
					game.get_win_vector ()
					print game


class ClassicBoard(Board):
	def __init__ (self):
		Board.__init__ (self)
		self.add (CashResult (1400), CashResult (1750), CashResult (2250))
		self.add (PrizeResult (), CashResult (500), CashResult (1250))
		self.add (WhammyResult (), CashResult (500), CashResult (2000))
		self.add (CashSpinResult (3000), CashSpinResult (4000), CashSpinResult (5000))
		self.add (WhammyResult (), PrizeResult (), CashResult (750))
		self.add (CashSpinResult (700), CashSpinResult (4000), CashResult (1500))

		self.add (WhammyResult (), PrizeResult (), CashResult (750))
		self.add (CashSpinResult (500), CashSpinResult (750), CashSpinResult (1000))
		self.add (WhammyResult (), CashResult (800), CashResult (1500))

		self.add (CashResult (2000), CashResult (2500), PrizeResult ())
		self.add (CashResult (1500), CashResult (2500), WhammyResult ())
		self.add (CashResult (500), WhammyResult (), CashSpinResult (4000))
		self.add (CashResult (2000), CashResult (2500), PrizeResult ())
		self.add (CashResult (1500), WhammyResult (), CashSpinResult (1000))
		self.add (CashSpinResult (1000), CashSpinResult (1500), PrizeResult ())

		self.add (CashSpinResult (750), CashResult (600), CashResult (2500))
		self.add (CashSpinResult (700), WhammyResult (), CashResult (600))
		self.add (CashSpinResult (750), CashSpinResult (1000), WhammyResult ())

		self.complete ()
		self.dump ()


# Entry point into the simulation.
# Right now, it is hardcoded as to what to do... change the code below
# to do what you want.
if __name__ == '__main__':

	# g = Endgame (2000, 5000)
	# g.player_up ().whammies = 3
	
	#EndgameTester ()

	# First, you need to create a Board object.  The subclass 'ClassicBoard'
	# will give you an instance of one of the actual game boards used on the
	# show for most of its run.
	board=ClassicBoard ()
	
	# For debugging, you can inspect the board object.
	#board.dump ()
	#board.test_win_prob ()

	# Then you create a Game simulation, and tie it to the gameboard just created.
	# Also you initialize the scores and spin counts of all players.  The
	# game action should always begin as DECIDING_PLAY.  Call g.initialize()
	# once everything has been set up.
	g = Game (board)
	g.action = DECIDING_PLAY
	g.player[0].score = 5000
	g.player[0].earned = 3
	g.player[1].score = 2000
	g.player[2].score = 10000
	g.initialize ()

	# The method 'get_win_vector' begins the simulation, and will return the
	# probability of the player up winning the game.  Note there is much logging
	# during the simulation so lots more gets printed too.
	print g.get_win_vector ()
	print "------"

	# The best play is saved in the game object, by printing it we will see
	# what we are supposed to do.
	print g

