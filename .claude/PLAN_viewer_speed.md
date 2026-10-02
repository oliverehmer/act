
### Umsetzung Runde 5 (2026-10-02)
- iclo e34921b: Fehler 1+2, Doppelbauten (kanonische Tier-Reihenfolge,
  Raw-Ansicht mit Unchanged-Schluessel), Log/Bauzeit nur bei echtem Bau, tote
  Reste entfernt. Offen: 300-ms-Luecke (Export, Class-Treffer), Cache-Grenze
  nach Anzahl bei render_all.
- act: Guard `nrow(mm_matches) > 0` um die drei Vorab-Durchlaeufe
  (layout_engine.R ~121). Identitaetstest erweitert um die Sequenz-Transkripte
  204_008 (7 mm-Tiers, 125 Zuordnungen) und 206_003 (10 mm-Tiers, 62) samt
  DOCX; 25/25 identisch, Engine-Zeit des Tests 99,7 s -> 29,9 s.
- Testskript-Hinweis: .claude/viewer_test_start_icas203_classes.R laedt den
  Class-Korpus nicht als Session-Korpus (class = FALSE); Kopie mit TRUE im
  Scratchpad benutzt.
