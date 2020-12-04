;;;; CYCO package exports
;;;;

(export '(*cyco-config-directory*
	  *cyco-config-file*
	  *cyco-config-profile*
	  *default-osc-send-address*
	  *default-osc-send-host*
	  *default-osc-send-port*
	  *local-host*
	  *metronome*
	  *osc-receive-address*
	  *osc-receive-buffer-length*
	  *osc-receive-port*
	  *osc-send-verbose*
	  *project*
	  *root-instrument*
	  +REST+
	  +default-articulation-map+
	  +default-dynamic-map+
	  +default-keynumber-map+
	  ->alist
	  ->cycle
	  ->list
	  ->markov-link
	  ->midi
	  ->pattern
	  ->string
	  ->symbol
	  ->vector
	  ?meta-channels
	  add-group
	  alist-p
	  append-filename-extension
	  approximate
	  articulation-map
	  articulation-map!
	  bag
	  bag-p
	  bar
	  bar-duration
	  bars
	  bars!
	  basic-articulation-map
	  basic-dynamic-map
	  basic-keynumber-map
	  beat-duration
	  beats
	  beats!
	  bend->midi-data
	  bool
	  bpm->beat-period
	  bpm->microseconds
	  build-cyco
	  butfinal
	  cardinality
	  channel
	  channel!
	  channel-index
	  channel-name
	  child-of-p
	  chord-model-p
	  chord-template
	  chord-types
	  circular-keynumber-map
	  clone
	  cnth
	  coin
	  connect
	  constant
	  constant-articulation-map
	  constant-function
	  copies
	  cycle
	  cycle-p
	  cyco
	  cyco-error
	  cyco-node
	  cyco-warning
	  data
	  data-count
	  defdynamic
	  define-chord
	  define-controller
	  defined-controllers
	  defines-chord-p
	  defkeynumber
	  dice
	  dice-p
	  disconnect
	  dismiss
	  dump-chords
	  dump-events
	  dump-smf-track-chunk
	  duration
	  dynamic
	  dynamic->velocity
	  dynamic-map
	  dynamic-map!
	  dynamic-name
	  dynamic-p
	  elide
	  exit
	  false
	  fill-list
	  final
	  find-child
	  flatten
	  flatten1
	  float-bar
	  get-controller-number
	  ghost
	  ghost-p
	  global
	  group-p
	  has-group-p
	  hash-property
	  init-time-signature
	  instrument
	  instrument-layer
	  instrument-layer-p
	  instrument-p
	  int->midi-vlv
	  invert
	  join-path
	  join-path-list
	  keyname
	  keynumber
	  keynumber-map
	  keynumber-map!
	  keynumber-p
	  limit
	  line
	  line-p
	  load-profile
	  load-profile-file
	  load-sub-profile
	  local-properties
	  make-ghost
	  make-instrument
	  markov-add-link
	  markov-chain-p
	  markov-link-p
	  markov-walk
	  mean
	  meta-channel
	  meta-channel!
	  meta-channel-assignment-p
	  metric
	  metric-expression
	  metric-expression-p
	  metric-p
	  metronome-articulation-map
	  metronome-dynamic-map
	  metronome-keynumber-map
	  midi-channel-message
	  midi-channel-message-p
	  midi-channel-pressure
	  midi-channel-pressure-p
	  midi-control-change
	  midi-control-chnage-p
	  midi-data->bend
	  midi-data->norm
	  midi-end-of-track-p
	  midi-end-system-exclusive
	  midi-end-system-exclusive-p
	  midi-key-message
	  midi-key-message-p
	  midi-key-signature
	  midi-key-signature-p
	  midi-message
	  midi-message-p
	  midi-meta-copyright
	  midi-meta-copyright-p
	  midi-meta-cue
	  midi-meta-cue-p
	  midi-meta-instrument-name
	  midi-meta-instrument-name-p
	  midi-meta-lyric
	  midi-meta-lyric-p
	  midi-meta-marker
	  midi-meta-marker-p
	  midi-meta-message
	  midi-meta-message-p
	  midi-meta-text
	  midi-meta-text-p
	  midi-meta-track-name
	  midi-meta-track-name-p
	  midi-note-off
	  midi-note-off-p
	  midi-note-on
	  midi-note-on-p
	  midi-pitch-bend
	  midi-pitch-bend-p
	  midi-poly-pressure
	  midi-poly-pressure-p
	  midi-program-change
	  midi-program-change-p
	  midi-system-common-message
	  midi-system-common-message-p
	  midi-system-exclusive
	  midi-system-exclusive-p
	  midi-tempo-message
	  midi-tempo-message-p
	  midi-time-signature
	  midi-time-signature-p
	  midi-vlv->int
	  mnemonic
	  mute
	  mute-all
	  muted-p
	  name
	  name!
	  next
	  next-1
	  next-n
	  norm->midi-data
	  note-events
	  null-program-map
	  octave
	  osc-send
	  palindrome
	  param
	  parent
	  part-p
	  partition-list
	  path-to-root
	  pattern-p
	  period
	  permute
	  phrase-duration
	  pick
	  pitch-class
	  print-tree
	  priority
	  program-bank
	  program-bank!
	  program-change-events
	  program-map
	  program-map!
	  program-number
	  program-number!
	  project->midi
	  project-p
	  properties
	  property
	  property*
	  prune
	  prune-orchestra
	  prune-project
	  pulse
	  push-event
	  put
	  ramp
	  read-midi-bend
	  read-midi-data
	  read-midi-long
	  read-midi-vlv
	  read-signed-midi-data
	  remaining
	  remarks
	  remarks!
	  render-midi-message
	  render-n
	  render-once
	  render-smf
	  render-smf-header
	  render-smf-track
	  reset
	  reset-channel-assignments
	  resolve-user-home
	  rest-p
	  retrograde
	  rl
	  root-p
	  rotate
	  sawtooth
	  section-order
	  section-p
	  set-basic-program-map
	  set-cyco-prompt
	  set-symbolic-program-map
	  sformat
	  signed-norm->midi-data
	  slice
	  smf
	  smf-track
	  smf-track!
	  smf-track-count
	  soft-reset
	  solo
	  sort-midi-events
	  split-list
	  split-string
	  strummer
	  strummer-p
	  subbeat-duration
	  subbeats
	  subbeats!
	  symbolic-keynumber-map
	  tbeat-duration
	  tbeats
	  tempo
	  tempo!
	  tick-duration
	  ticks-per-beat
	  time-signature
	  transpose
	  triangle
	  true
	  tsubbeat-duration
	  tsubbeats
	  unit
	  unit!
	  unmute
	  unmute-all
	  walker
	  walker-p
	  while
	  wrapper
	  wrapper-p
	  write-smf
	  zip) :cyco)
