;;;; CYCO package exports
;;;;

(export '(*chord-table*
	  *cyco-config-directory*
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
          +root-instrument+
	  +REST+
	  +default-articulation-map+
	  +default-dynamic-map+
	  +default-keynumber-map+
	  ->alist
	  ->cycle
	  ->list
	  ->midi
	  ->pattern
	  ->string
	  ->symbol
	  ->vector
	  ?meta-channels
	  add-group
	  alist-p
	  alloy
	  append-filename-extension
	  approximate
	  articulation-map
	  asr-envelope
	  bag
	  bag-p
	  bar
	  bar-duration
	  bars
	  basic-articulation-map
	  basic-dynamic-map
	  basic-keynumber-map
	  beat-duration
	  beats
	  bend->midi-data
	  bones
	  bool
	  bpm->beat-period
	  bpm->microseconds
	  build-cyco
	  butfinal
	  channel
	  channel-index
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
	  controller-p
	  copies
	  counter
	  cue
	  cuelist
	  cue-gamut
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
	  duck
	  dump-chords
	  dump-events
	  dump-smf-track-chunk
	  duration
	  dynamic
	  dynamic->velocity
	  dynamic-map
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
	  generator
	  get-controller-number
	  ghost
	  ghost-p
	  global
	  group-p
	  hailstone
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
	  keynumber-p
	  limit
	  line
	  line-p
	  load-profile
	  load-profile-file
	  load-sub-profile
	  local-properties
	  logistic
	  make-ghost
	  make-instrument
	  mean
	  meta-channel
	  meta-channel-assignment-p
	  meta-channel-names
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
	  midi-clock
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
	  n-complement
	  name
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
	  pattern-comprehension
	  pattern-comprehension-p
	  pattern-length
	  pattern-p
	  period
	  permute
	  phrase-duration
	  pick
	  pitch-class
	  print-tree
	  pprint-cuelist
	  priority
	  program-bank
	  program-change-events
	  program-map
	  program-number
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
	  recaman
	  remaining
	  remarks
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
	  sample-and-hold
	  sawtooth
	  section-order
	  section-p
	  select
	  set-articulation-map
	  set-bars
	  set-basic-program-map
	  set-beats
	  set-channel
	  set-cyco-prompt
	  set-dynamic-map
	  set-keynumber-map
	  set-meta-channel
	  set-name
	  set-program-bank
	  set-program-map
	  set-program-number
	  set-remarks
	  set-smf-track
	  set-subbeats
	  set-symbolic-program-map
	  set-tempo
	  set-unit
	  sformat
	  shift-register
	  signed-norm->midi-data
	  slice
	  slowglass
	  smf
	  smf-track
	  smf-track-count
	  soft-reset
	  solo
	  sort-midi-events
	  split-list
	  split-string
	  str+
	  strummer
	  strummer-p
	  subbeat-duration
	  subbeats
	  switch
	  symbolic-keynumber-map
	  tbar
	  tbeat-duration
	  tbeats
	  tempo
	  thin-bend-events
	  thin-controller-events
	  tick-duration
	  ticks-per-beat
	  time-signature
	  transpose
	  triangle
	  true
	  tsubbeat-duration
	  tsubbeats
	  unit
	  unmute
	  unmute-all
	  walker
	  walker-p
	  while
	  wrapper
	  wrapper-p
	  write-smf
	  zip) :cyco)
