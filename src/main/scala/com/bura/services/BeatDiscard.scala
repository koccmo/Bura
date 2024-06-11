package com.bura.services

import com.bura.domain.{Card, Player}

case class BeatDiscard(player: Player, defendCards: List[Card], hiddenDiscard: List[Card])
