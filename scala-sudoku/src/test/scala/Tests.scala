class TestSuite extends org.scalatest.FunSuite {

import Solution._
import hw.sudoku._

  test("The solution object must be defined") {
    val obj : hw.sudoku.SudokuLike = Solution
  }

  test("peers work"){
		assert(peers(0,0) == List((1,0), (2,0), (3,0), (4,0), (5,0), (6,0), (7,0), (8,0), (0,1), (0,2), (0,3), (0,4), (0,5), (0,6), (0,7), (0,8), (1,1), (1,2), (2,1), (2,2)))
	}
	val str = "....8.3...6..7..84.3.5..2.9...1.54.8.........4.27.6...3.1..7.4.72..4..6...4.1...3"
	val empty_brd = "................................................................................."
	
	test("nextStates properly sorted"){
		
		val statesList = parse(str).nextStates
		assert(!statesList.isEmpty || (statesList, statesList.tail).zipped.forall{
			case (board1,board2) =>
	    		val board1count = board1.available.foldLeft(0){
           		case (a: Int,(k, v : List[Int])) if v.length != 1 =>
	            	a + v.length
            	case (a: Int, (k,v: List[Int])) => a
	        	}
	        	val board2count = board2.available.foldLeft(0){
	          	case (a: Int,(k, v: List[Int])) if v.length != 1 =>
	            	a + v.length
            	case (a: Int, (k,v: List[Int])) => a
	        	}

	        	board1count <= board2count
			})
	}

	test("valueAt works"){
		assert(parse(str).valueAt(0,4) == Some(8))
	}

	test("solve works"){
		val fromCS121_1 = "85....4.1......67...21....3..85....7...982...3....15..5....43...37......2.9....58"
		val fromCS121_2 = ".1.....2..3..9..1656..7...33.7..8..........89....6......6.254..9.5..1..7..3.....2"
		val puz1 = ".43.8.25.6.............1.949....4.7....6.8....1.2....382.5.............5.34.9.71."
		val puz2 = "2...8.3...6..7..84.3.5..2.9...1.54.8.........4.27.6...3.1..7.4.72..4..6...4.1...3"
		val s = System.currentTimeMillis
		val s1 = parse(fromCS121_1).solve
		val s2 = parse(fromCS121_2).solve
		val s3 = parse(puz1).solve
		val s4 = parse(puz2).solve

		val s1s = s1 match { case Some(a: Board) => a 
			case None => parse(empty_brd)}
		val s2s = s2 match { case Some(a: Board) => a 
			case None => parse(empty_brd)}
		val s3s = s3 match { case Some(a: Board) => a 
			case None => parse(empty_brd)}
		val s4s = s4 match { case Some(a: Board) => a 
			case None => parse(empty_brd)}

		assert(s1 match { case Some(a) => true
			case _ => false })
		assert(s2 match { case Some(a) => true
			case _ => false })
		assert(s3 match { case Some(a) => true
			case _ => false })
		assert(s4 match { case Some(a) => true
			case _ => false })
	}

    test("puzzle 1") {
    assert(parse("85....4.1......67...21....3..85....7...982...3....15..5....43...37......2.9....58").solve.get.isSolved)}
    test("puzzle 2") {
    assert(parse(".1.....2..3..9..1656..7...33.7..8..........89....6......6.254..9.5..1..7..3.....2").solve.get.isSolved)}
    test("puzzle 3") {
    assert(parse(".43.8.25.6.............1.949....4.7....6.8....1.2....382.5.............5.34.9.71.").solve.get.isSolved)}
    test("puzzle 4") {
    assert(parse("2...8.3...6..7..84.3.5..2.9...1.54.8.........4.27.6...3.1..7.4.72..4..6...4.1...3").solve.get.isSolved)}
    test("puzzle 5") {
    assert(parse("..3.2.6..9..3.5..1..18.64....81.29..7.......8..67.82....26.95..8..2.3..9..5.1.3..").solve.get.isSolved)}
    test("puzzle 6") {
    assert(parse(".2.81.74.7....31...9...28.5..9.4..874..2.8..316..3.2..3.27...6...56....8.76.51.9.").solve.get.isSolved)}
    test("puzzle 7") {
    assert(parse(".3..5..4...8.1.5..46.....12.7.5.2.8....6.3....4.1.9.3.25.....98..1.2.6...8..6..2.").solve.get.isSolved)}
    test("puzzle 8") {
    assert(parse(".43.8.25.6.............1.949....4.7....6.8....1.2....382.5.............5.34.9.71.").solve.get.isSolved)}
    test("puzzle 9") {
    assert(parse("48...69.2..2..8..19..37..6.84..1.2....37.41....1.6..49.2..85..77..9..6..6.92...18").solve.get.isSolved)}
    test("puzzle 10") {
    assert(parse("...1254....84.....42.8......3.....95.6.9.2.1.51.....6......3.49.....72....1298...").solve.get.isSolved)}
    test("puzzle 11") {
    assert(parse(".6234.75.1....56..57.....4.....948..4.......6..583.....3.....91..64....7.59.8326.").solve.get.isSolved)}
    test("puzzle 12") {
    assert(parse("3..........5..9...2..5.4....2....7..16.....587.431.6.....89.1......67.8......5437").solve.get.isSolved)}
    test("puzzle 13") {
    assert(parse("....2..4...8.35.......7.6.2.31.4697.2...........5.12.3.49...73........1.8....4...").solve.get.isSolved)}
    test("puzzle 14") {
    assert(parse("361.259...8.96..1.4......57..8...471...6.3...259...8..74......5.2..18.6...547.329").solve.get.isSolved)}
    test("puzzle 15") {
    assert(parse(".5.8.7.2.6...1..9.7.254...6.7..2.3.15.4...9.81.3.8..7.9...762.5.6..9...3.8.1.3.4.").solve.get.isSolved)}
    test("puzzle 16") {
    assert(parse(".8...5........3457....7.8.9.6.4..9.3..7.1.5..4.8..7.2.9.1.2....8423........1...8.").solve.get.isSolved)}
    test("puzzle 17") {
    assert(parse("..35.29......4....1.6...3.59..251..8.7.4.8.3.8..763..13.8...1.4....2......51.48..").solve.get.isSolved)}
    test("puzzle 18") {
    assert(parse("...........98.51...519.742.29.4.1.65.........14.5.8.93.267.958...51.36...........").solve.get.isSolved)}
    test("puzzle 19") {
    assert(parse(".2..3..9....9.7...9..2.8..5..48.65..6.7...2.8..31.29..8..6.5..7...3.9....3..2..5.").solve.get.isSolved)}
    test("puzzle 20") {
    assert(parse("..5.....6.7...9.2....5..1.78.415.......8.3.......928.59.7..6....3.4...1.2.....6..").solve.get.isSolved)}
    test("puzzle 21") {
    assert(parse(".4.....5...19436....9...3..6...5...21.3...5.68...2...7..5...2....24367...3.....4.").solve.get.isSolved)}
    test("puzzle 22") {
    assert(parse("..4..........3...239.7...8.4....9..12.98.13.76..2....8.1...8.539...4..........8..").solve.get.isSolved)}
    test("puzzle 23") {
    assert(parse("..72564..4.......5.1..3..6....5.8.....8.6.2.....1.7....3..7..9.2.......4..63127..").solve.get.isSolved)}
    test("puzzle 24") {
    assert(parse("..........79.5.18.8.......7..73.68..45.7.8.96..35.27..7.......5.16.3.42..........").solve.get.isSolved)}
    test("puzzle 25") {
    assert(parse(".3.....8...9...5....75.92..7..1.5..8.2..9..3.9..4.2..1..42.71....2...8...7.....9.").solve.get.isSolved)}
    test("puzzle 26") {
    assert(parse("2..17.6.3.5....1.......6.79....4.7.....8.1.....9.5....31.4.......5....6.9.6.37..2").solve.get.isSolved)}
    test("puzzle 27") {
    assert(parse(".......8.8..7.1.4..4..2..3.374...9......3......5...321.1..6..5..5.8.2..6.8.......").solve.get.isSolved)}
    test("puzzle 28") {
    assert(parse("6.8.7.5.2.5.6.8.7...2...3..5...9...6.4.3.2.5.8...5...3..5...2...1.7.4.9.4.9.6.7.1").solve.get.isSolved)}
    test("puzzle 29") {
    assert(parse(".5..1..4.1.7...6.2...9.5...2.8.3.5.1.4..7..2.9.1.8.4.6...4.1...3.4...7.9.2..6..1.").solve.get.isSolved)}
    test("puzzle 30") {
    assert(parse(".53...79...97534..1.......2.9..8..1....9.7....8..3..7.5.......3..76412...61...94.").solve.get.isSolved)}
    test("puzzle 31") {
    assert(parse("..6.8.3...49.7.25....4.5...6..317..4..7...8..1..826..9...7.2....75.4.19...3.9.6..").solve.get.isSolved)}
    test("puzzle 32") {
    assert(parse("..5.8.7..7..2.4..532.....84.6.1.5.4...8...5...7.8.3.1.45.....916..5.8..7..3.1.6..").solve.get.isSolved)}
    test("puzzle 33") {
    assert(parse("...9..8..128..64...7.8...6.8..43...75.......96...79..8.9...4.1...36..284..1..7...").solve.get.isSolved)}
    test("puzzle 34") {
    assert(parse("....8....27.....54.95...81...98.64...2.4.3.6...69.51...17...62.46.....38....9....").solve.get.isSolved)}
    test("puzzle 35") {
    assert(parse("...6.2...4...5...1.85.1.62..382.671...........194.735..26.4.53.9...2...7...8.9...").solve.get.isSolved)}
    test("puzzle 36") {
    assert(parse("38..........4..785..9.2.3...6..9....8..3.2..9....4..7...1.7.5..495..6..........92").solve.get.isSolved)}
    test("puzzle 37") {
    assert(parse("...158.....2.6.8...3.....4..27.3.51...........46.8.79..5.....8...4.7.1.....325...").solve.get.isSolved)}
    test("puzzle 38") {
    assert(parse(".8.....4....469...4.......7..59.46...7.6.8.3...85.21..9.......5...781....6.....1.").solve.get.isSolved)}


}