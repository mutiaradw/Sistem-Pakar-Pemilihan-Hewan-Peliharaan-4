:- use_module(library(lists)).
:- use_module(library(random)).

:- dynamic(yes/1).
:- dynamic(no/1).

pet_rule(hamster, cocok_apartemen_sibuk,
    [ukuran_tempat_tinggal_apartemen, waktu_luang_sedikit, anggaran_bulanan_rendah,
     interaksi_rendah, komitmen_jangka_pendek, ukuran_hewan_kecil]).
pet_rule(hamster, pemula_low_maintenance,
    [pengguna_adalah_pemula, anggaran_bulanan_rendah, ukuran_hewan_kecil, grooming_rendah]).

pet_rule(ikan_cupang, apartemen_low_budget_alergi,
    [ukuran_tempat_tinggal_apartemen, anggaran_bulanan_rendah, toleransi_bising_rendah,
     interaksi_rendah, alergi_ringan]).
pet_rule(ikan_cupang, pemula_estetik_low_budget,
    [pengguna_adalah_pemula, anggaran_bulanan_rendah, toleransi_bising_rendah, sifat_estetik]).

pet_rule(burung_kenari, rumah_kecil_suka_suara,
    [ukuran_tempat_tinggal_rumah_kecil, toleransi_bising_tinggi, anggaran_bulanan_sedang, interaksi_sedang]).
pet_rule(burung_kenari, pasif_suka_suara,
    [waktu_luang_sedikit, toleransi_bising_tinggi, interaksi_rendah]).

pet_rule(kelinci, halaman_anak_tenang,
    [ada_halaman, anggaran_bulanan_sedang, ada_anak_kecil, interaksi_sedang, aktivitas_hewan_tenang]).
pet_rule(kelinci, apartemen_interaksi_ruang,
    [ukuran_tempat_tinggal_apartemen, waktu_luang_sedang, interaksi_sedang, butuh_ruang_gerak_cukup]).

pet_rule(kucing_domestik, rumah_kecil_interaktif_mandiri,
    [ukuran_tempat_tinggal_rumah_kecil, waktu_luang_sedang, anggaran_bulanan_sedang,
     interaksi_tinggi, sifat_mandiri]).
pet_rule(kucing_domestik, apartemen_mandiri_grooming_rendah,
    [ukuran_tempat_tinggal_apartemen, waktu_luang_sedang, grooming_rendah, sifat_mandiri]).
pet_rule(kucing_domestik, pemula_no_alergi_mudah_rawat,
    [alergi_tidak_ada, pengguna_adalah_pemula, anggaran_bulanan_sedang, grooming_rendah]).

pet_rule(anjing_golden_retriever, rumah_besar_aktif_keluarga,
    [ukuran_tempat_tinggal_rumah_besar, ada_halaman, waktu_luang_banyak, anggaran_bulanan_tinggi,
     interaksi_tinggi, aktivitas_hewan_aktif, bukan_muslim]).
pet_rule(anjing_golden_retriever, halaman_anak_besar_interaktif,
    [ada_halaman, waktu_luang_banyak, ada_anak_kecil, ukuran_hewan_besar,
     interaksi_tinggi, bukan_muslim]).

pet_rule(anjing_beagle, rumah_kecil_halaman_aktif_berisik,
    [ukuran_tempat_tinggal_rumah_kecil, ada_halaman, waktu_luang_banyak, toleransi_bising_tinggi,
     aktivitas_hewan_aktif, bukan_muslim]).
pet_rule(anjing_beagle, halaman_budget_sedang_interaktif,
    [ada_halaman, anggaran_bulanan_sedang, interaksi_tinggi, ukuran_hewan_sedang, bukan_muslim]).

pet_rule(anjing_pomeranian, apartemen_aktif_vokal_kecil,
    [ukuran_tempat_tinggal_apartemen, waktu_luang_sedang, anggaran_bulanan_sedang, interaksi_tinggi,
     ukuran_hewan_kecil, toleransi_bising_tinggi, bukan_muslim]).

pet_rule(kura_kura, apartemen_low_maintenance_jangka_panjang,
    [ukuran_tempat_tinggal_apartemen, waktu_luang_sedikit, anggaran_bulanan_rendah, interaksi_rendah,
     komitmen_jangka_panjang, ukuran_hewan_kecil, toleransi_bising_rendah]).

pet_rule(burung_parkit, apartemen_sosial_bicara,
    [ukuran_tempat_tinggal_apartemen, waktu_luang_sedang, anggaran_bulanan_rendah, interaksi_sedang,
     toleransi_bising_tinggi]).

pet_rule(sugar_glider, nokturnal_sosial_berpengalaman,
    [pengguna_bukan_pemula, waktu_luang_banyak, anggaran_bulanan_tinggi, interaksi_tinggi,
     komitmen_jangka_panjang, sifat_aktif_malam]).

pet_rule(hamster_syrian, apartemen_soliter_low_maintenance,
    [ukuran_tempat_tinggal_apartemen, waktu_luang_sedikit, anggaran_bulanan_rendah, interaksi_rendah,
     ukuran_hewan_kecil, sifat_soliter]).

pet_rule(kucing_persia, apartemen_grooming_tinggi_tenang,
    [ukuran_tempat_tinggal_apartemen, waktu_luang_sedang, anggaran_bulanan_sedang, interaksi_sedang,
     grooming_tinggi, aktivitas_hewan_tenang, alergi_ringan]).

pet_rule(leopard_gecko, reptil_pemula_nokturnal_tenang,
    [ukuran_tempat_tinggal_apartemen, waktu_luang_sedikit, anggaran_bulanan_rendah, interaksi_rendah,
     komitmen_jangka_panjang, ukuran_hewan_kecil, sifat_aktif_malam]).

fallback_pet_justification(ikan_cupang, fallback_cupang,
    ['Perawatan relatif rendah dan tidak membutuhkan banyak ruang.',
     'Visualnya menarik dan dapat menjadi hiasan yang indah.',
     'Cocok untuk pemula atau yang memiliki keterbatasan waktu dan tempat.']).
fallback_pet_justification(hamster, fallback_hamster,
    ['Ukurannya kecil, ideal untuk apartemen atau kamar.',
     'Perawatannya relatif mudah dan biayanya terjangkau.',
     'Bisa menjadi hewan yang menghibur untuk diamati.']).
fallback_pet_justification(kucing_domestik, fallback_kucing,
    ['Dapat beradaptasi dengan baik di dalam ruangan (indoor).',
     'Banyak kucing memiliki sifat mandiri namun tetap penyayang.',
     'Perawatan bervariasi tergantung jenis bulu, tetapi banyak yang perawatannya mudah.']).
fallback_pet_justification(kura_kura, fallback_kura_kura,
    ['Dikenal memiliki umur yang panjang.',
     'Merupakan hewan yang tenang dan tidak berisik.',
     'Perawatannya bisa sederhana jika habitatnya sudah disiapkan dengan benar.']).

verify_all([]).
verify_all([Criterion|Criteria]) :-
    verify(Criterion),
    verify_all(Criteria).

recommended_pet(Pet, JustificationTextList) :-
    pet_rule(Pet, _RuleId, CriteriaAtomList),
    verify_all(CriteriaAtomList),
    map_criteria_to_text(CriteriaAtomList, JustificationTextList).

map_criteria_to_text([], []).
map_criteria_to_text([CritAtom|RestAtoms], [CritText|RestTexts]) :-
    (criteria_explanation_text(CritAtom, Text) -> CritText = Text ; CritText = CritAtom),
    map_criteria_to_text(RestAtoms, RestTexts).

ask_choice(Question, Options, Chosen) :-
    nl, write(Question), nl,
    display_options(Options, 1),
    write('Pilihan Anda (nomor): '),
    flush_output,
    read_line_to_string(user_input, R_Str),
    catch(atom_number(R_Str, R_Num), _, R_Num = -1),
    (nth1(R_Num, Options, ChosenAtom) ->
        Chosen = ChosenAtom,
        assert(yes(ChosenAtom)),
        assert_no_others(Options, ChosenAtom)
    ;   nl, write('-----------------------------------------------------'), nl,
        write('Input tidak valid! Harap masukkan nomor pilihan yang tersedia.'), nl,
        write('-----------------------------------------------------'), nl,
        ask_choice(Question, Options, Chosen)
    ).

ask_yes_no(Question, Symptom) :-
    format('~w? (y/n) ~n', [Question]), 
    flush_output,                      
    read_line_to_string(user_input, R_Str_With_Newline),
    ( current_predicate(string_trim/2) ->
        string_trim(R_Str_With_Newline, Trimmed_R_Str)
    ;   % Fallback jika string_trim tidak ada (misalnya, versi Prolog sangat lama)
        % Ini cara sederhana untuk mencoba menghapus newline di akhir jika itu satu-satunya masalah
        ( sub_string(R_Str_With_Newline, Before, 1, 0, "\n") ->
            sub_string(R_Str_With_Newline, 0, Before, _, Trimmed_R_Str)
        ;   Trimmed_R_Str = R_Str_With_Newline
        )
    ),
    string_lower(Trimmed_R_Str, LowerR),
    ( (LowerR == "y" ; LowerR == "yes") -> assert(yes(Symptom))
    ; (LowerR == "n" ; LowerR == "no") -> assert(no(Symptom)), fail
    ;   nl, write('-----------------------------------------------------'), nl,
        write('Jawaban tidak valid. Harap jawab y (untuk Ya) atau n (untuk Tidak).'), nl,
        write('-----------------------------------------------------'), nl,
        ask_yes_no(Question, Symptom)
    ).


display_options([], _).
display_options([H|T], N) :-
    ( option_text(H, Text) -> format('  ~d. ~w~n', [N, Text])
    ; format('  ~d. ~w~n', [N, H])
    ),
    N1 is N + 1,
    display_options(T, N1).

option_text(pengguna_adalah_pemula, 'Ya, saya seorang pemula').
option_text(pengguna_bukan_pemula, 'Tidak, saya bukan pemula (berpengalaman)').

option_text(muslim_ya, 'Ya, saya seorang Muslim').
option_text(bukan_muslim, 'Tidak, saya bukan Muslim').

option_text(ukuran_tempat_tinggal_apartemen, 'Apartemen').
option_text(ukuran_tempat_tinggal_rumah_kecil, 'Rumah Kecil (tipe 36-45)').
option_text(ukuran_tempat_tinggal_rumah_besar, 'Rumah Besar (> tipe 45 atau dengan halaman luas)').

option_text(ada_halaman, 'Ya, ada halaman').
option_text(tidak_ada_halaman, 'Tidak, tidak ada halaman').

option_text(waktu_luang_sedikit, 'Sedikit (< 1 jam per hari)').
option_text(waktu_luang_sedang, 'Sedang (1-2 jam per hari)').
option_text(waktu_luang_banyak, 'Banyak (> 2 jam per hari)').

option_text(anggaran_bulanan_rendah, 'Rendah (< Rp200.000 per bulan)').
option_text(anggaran_bulanan_sedang, 'Sedang (Rp200.000 - Rp500.000 per bulan)').
option_text(anggaran_bulanan_tinggi, 'Tinggi (> Rp500.000 per bulan)').

option_text(alergi_tidak_ada, 'Tidak ada yang alergi').
option_text(alergi_ringan, 'Ada alergi ringan (bersin, gatal ringan)').
option_text(alergi_berat, 'Ada alergi berat (sesak napas, ruam parah)').

option_text(toleransi_bising_rendah, 'Rendah (lebih suka suasana tenang)').
option_text(toleransi_bising_sedang, 'Sedang (sedikit kebisingan tidak masalah)').
option_text(toleransi_bising_tinggi, 'Tinggi (kebisingan cukup dapat ditoleransi)').

option_text(interaksi_rendah, 'Rendah (lebih suka hewan yang mandiri/diamati)').
option_text(interaksi_sedang, 'Sedang (senang berinteraksi tapi tidak terus-menerus)').
option_text(interaksi_tinggi, 'Tinggi (ingin hewan yang sangat interaktif dan sering diajak bermain)').

option_text(ada_anak_kecil, 'Ya, ada anak kecil (di bawah 7 tahun)').
option_text(tidak_ada_anak_kecil, 'Tidak, tidak ada anak kecil').

option_text(komitmen_jangka_pendek, 'Jangka Pendek (1-3 tahun)').
option_text(komitmen_jangka_menengah, 'Jangka Menengah (4-8 tahun)').
option_text(komitmen_jangka_panjang, 'Jangka Panjang (>8 tahun)').

option_text(aktivitas_hewan_aktif, 'Aktif (suka hewan yang banyak bergerak dan bermain)').
option_text(aktivitas_hewan_tenang, 'Tenang (suka hewan yang kalem dan tidak banyak bergerak)').

option_text(grooming_rendah, 'Rendah (seminggu sekali atau kurang, atau tidak perlu grooming khusus)').
option_text(grooming_sedang, 'Sedang (beberapa kali seminggu)').
option_text(grooming_tinggi, 'Tinggi (setiap hari atau butuh keahlian khusus)').

option_text(ukuran_hewan_kecil, 'Kecil (seukuran hamster, ikan cupang)').
option_text(ukuran_hewan_sedang, 'Sedang (seukuran kucing, anjing beagle)').
option_text(ukuran_hewan_besar, 'Besar (seukuran anjing golden retriever)').

assert_no_others([], _).
assert_no_others([H|T], Chosen) :-
    ( H == Chosen -> true ; assert(no(H)) ),
    assert_no_others(T, Chosen).

verify(Criteria) :-
    (yes(Criteria) -> true ;
     no(Criteria) -> fail ;
     (criteria_question(Criteria, Type, Question, Options) ->
        ( (Type == yes_no -> ask_yes_no(Question, Criteria))
        ; (Type == choice -> ask_choice(Question, Options, _ChosenAtom))
        ),
        yes(Criteria)
     ;
        format('Warning: Pertanyaan untuk ~w tidak ditemukan, menganggap "tidak".~n', [Criteria]),
        assert(no(Criteria)), fail
     )
    ).

question_text(pengalaman, 'Apakah Anda seorang pemula dalam merawat hewan?').
question_text(muslim, 'Apakah Anda seorang Muslim? (Penting untuk rekomendasi jenis hewan tertentu)').
question_text(tipe_tempat_tinggal, 'Apa tipe tempat tinggal Anda?').
question_text(halaman, 'Apakah Anda memiliki halaman?').
question_text(waktu_luang, 'Berapa banyak waktu luang harian Anda untuk hewan peliharaan?').
question_text(anggaran_bulanan, 'Bagaimana perkiraan anggaran bulanan Anda untuk hewan peliharaan?').
question_text(alergi, 'Apakah ada anggota keluarga yang memiliki alergi terhadap bulu hewan?').
question_text(toleransi_bising, 'Bagaimana tingkat toleransi Anda terhadap kebisingan yang mungkin ditimbulkan hewan?').
question_text(tingkat_interaksi, 'Tingkat interaksi seperti apa yang Anda inginkan dengan hewan peliharaan?').
question_text(keberadaan_anak_kecil, 'Apakah ada anak kecil (di bawah 7 tahun) di rumah Anda?').
question_text(lama_komitmen, 'Untuk berapa lama Anda siap berkomitmen merawat hewan peliharaan (berdasarkan perkiraan usia hidup hewan)?').
question_text(tingkat_aktivitas_hewan, 'Anda lebih menyukai hewan peliharaan dengan tingkat aktivitas seperti apa?').
question_text(frekuensi_grooming, 'Seberapa sering Anda bersedia melakukan grooming (perawatan seperti menyisir bulu, memandikan, dll.)?').
question_text(ukuran_hewan, 'Ukuran hewan peliharaan seperti apa yang Anda inginkan?').

criteria_question(pengguna_adalah_pemula, choice, Q, [pengguna_adalah_pemula, pengguna_bukan_pemula]) :- question_text(pengalaman, Q).
criteria_question(pengguna_bukan_pemula, choice, Q, [pengguna_adalah_pemula, pengguna_bukan_pemula]) :- question_text(pengalaman, Q).

criteria_question(muslim_ya, choice, Q, [muslim_ya, bukan_muslim]) :- question_text(muslim, Q).
criteria_question(bukan_muslim, choice, Q, [muslim_ya, bukan_muslim]) :- question_text(muslim, Q).

criteria_question(ukuran_tempat_tinggal_apartemen, choice, Q, [ukuran_tempat_tinggal_apartemen, ukuran_tempat_tinggal_rumah_kecil, ukuran_tempat_tinggal_rumah_besar]) :- question_text(tipe_tempat_tinggal, Q).
criteria_question(ukuran_tempat_tinggal_rumah_kecil, choice, Q, [ukuran_tempat_tinggal_apartemen, ukuran_tempat_tinggal_rumah_kecil, ukuran_tempat_tinggal_rumah_besar]) :- question_text(tipe_tempat_tinggal, Q).
criteria_question(ukuran_tempat_tinggal_rumah_besar, choice, Q, [ukuran_tempat_tinggal_apartemen, ukuran_tempat_tinggal_rumah_kecil, ukuran_tempat_tinggal_rumah_besar]) :- question_text(tipe_tempat_tinggal, Q).

criteria_question(ada_halaman, choice, Q, [ada_halaman, tidak_ada_halaman]) :- question_text(halaman, Q).
criteria_question(tidak_ada_halaman, choice, Q, [ada_halaman, tidak_ada_halaman]) :- question_text(halaman, Q).

criteria_question(waktu_luang_sedikit, choice, Q, [waktu_luang_sedikit, waktu_luang_sedang, waktu_luang_banyak]) :- question_text(waktu_luang, Q).
criteria_question(waktu_luang_sedang, choice, Q, [waktu_luang_sedikit, waktu_luang_sedang, waktu_luang_banyak]) :- question_text(waktu_luang, Q).
criteria_question(waktu_luang_banyak, choice, Q, [waktu_luang_sedikit, waktu_luang_sedang, waktu_luang_banyak]) :- question_text(waktu_luang, Q).

criteria_question(anggaran_bulanan_rendah, choice, Q, [anggaran_bulanan_rendah, anggaran_bulanan_sedang, anggaran_bulanan_tinggi]) :- question_text(anggaran_bulanan, Q).
criteria_question(anggaran_bulanan_sedang, choice, Q, [anggaran_bulanan_rendah, anggaran_bulanan_sedang, anggaran_bulanan_tinggi]) :- question_text(anggaran_bulanan, Q).
criteria_question(anggaran_bulanan_tinggi, choice, Q, [anggaran_bulanan_rendah, anggaran_bulanan_sedang, anggaran_bulanan_tinggi]) :- question_text(anggaran_bulanan, Q).

criteria_question(alergi_ringan, choice, Q, [alergi_tidak_ada, alergi_ringan, alergi_berat]) :- question_text(alergi, Q).
criteria_question(alergi_tidak_ada, choice, Q, [alergi_tidak_ada, alergi_ringan, alergi_berat]) :- question_text(alergi, Q).
criteria_question(alergi_berat, choice, Q, [alergi_tidak_ada, alergi_ringan, alergi_berat]) :- question_text(alergi, Q).

criteria_question(toleransi_bising_rendah, choice, Q, [toleransi_bising_rendah, toleransi_bising_sedang, toleransi_bising_tinggi]) :- question_text(toleransi_bising, Q).
criteria_question(toleransi_bising_sedang, choice, Q, [toleransi_bising_rendah, toleransi_bising_sedang, toleransi_bising_tinggi]) :- question_text(toleransi_bising, Q).
criteria_question(toleransi_bising_tinggi, choice, Q, [toleransi_bising_rendah, toleransi_bising_sedang, toleransi_bising_tinggi]) :- question_text(toleransi_bising, Q).

criteria_question(interaksi_tinggi, choice, Q, [interaksi_tinggi, interaksi_sedang, interaksi_rendah]) :- question_text(tingkat_interaksi, Q).
criteria_question(interaksi_sedang, choice, Q, [interaksi_tinggi, interaksi_sedang, interaksi_rendah]) :- question_text(tingkat_interaksi, Q).
criteria_question(interaksi_rendah, choice, Q, [interaksi_tinggi, interaksi_sedang, interaksi_rendah]) :- question_text(tingkat_interaksi, Q).

criteria_question(ada_anak_kecil, choice, Q, [ada_anak_kecil, tidak_ada_anak_kecil]) :- question_text(keberadaan_anak_kecil, Q).
criteria_question(tidak_ada_anak_kecil, choice, Q, [ada_anak_kecil, tidak_ada_anak_kecil]) :- question_text(keberadaan_anak_kecil, Q).

criteria_question(komitmen_jangka_pendek, choice, Q, [komitmen_jangka_pendek, komitmen_jangka_menengah, komitmen_jangka_panjang]) :- question_text(lama_komitmen, Q).
criteria_question(komitmen_jangka_menengah, choice, Q, [komitmen_jangka_pendek, komitmen_jangka_menengah, komitmen_jangka_panjang]) :- question_text(lama_komitmen, Q).
criteria_question(komitmen_jangka_panjang, choice, Q, [komitmen_jangka_pendek, komitmen_jangka_menengah, komitmen_jangka_panjang]) :- question_text(lama_komitmen, Q).

criteria_question(aktivitas_hewan_aktif, choice, Q, [aktivitas_hewan_aktif, aktivitas_hewan_tenang]) :- question_text(tingkat_aktivitas_hewan, Q).
criteria_question(aktivitas_hewan_tenang, choice, Q, [aktivitas_hewan_aktif, aktivitas_hewan_tenang]) :- question_text(tingkat_aktivitas_hewan, Q).

criteria_question(grooming_rendah, choice, Q, [grooming_rendah, grooming_sedang, grooming_tinggi]) :- question_text(frekuensi_grooming, Q).
criteria_question(grooming_sedang, choice, Q, [grooming_rendah, grooming_sedang, grooming_tinggi]) :- question_text(frekuensi_grooming, Q).
criteria_question(grooming_tinggi, choice, Q, [grooming_rendah, grooming_sedang, grooming_tinggi]) :- question_text(frekuensi_grooming, Q).

criteria_question(ukuran_hewan_kecil, choice, Q, [ukuran_hewan_kecil, ukuran_hewan_sedang, ukuran_hewan_besar]) :- question_text(ukuran_hewan, Q).
criteria_question(ukuran_hewan_sedang, choice, Q, [ukuran_hewan_kecil, ukuran_hewan_sedang, ukuran_hewan_besar]) :- question_text(ukuran_hewan, Q).
criteria_question(ukuran_hewan_besar, choice, Q, [ukuran_hewan_kecil, ukuran_hewan_sedang, ukuran_hewan_besar]) :- question_text(ukuran_hewan, Q).

criteria_question(sifat_mandiri, yes_no, 'Apakah Anda mencari hewan yang cenderung mandiri dan tidak selalu butuh perhatian Anda', _).
criteria_question(sifat_aktif_malam, yes_no, 'Apakah Anda tidak masalah dengan hewan yang lebih aktif di malam hari (nokturnal)', _).
criteria_question(sifat_estetik, yes_no, 'Apakah Anda mencari hewan yang indah atau estetik untuk dilihat sebagai salah satu pertimbangan utama', _).
criteria_question(butuh_ruang_gerak_cukup, yes_no, 'Apakah Anda dapat menyediakan ruang gerak yang cukup di dalam rumah untuk hewan yang membutuhkannya', _).
criteria_question(sifat_soliter, yes_no, 'Apakah Anda mencari hewan yang cenderung soliter (lebih suka sendiri) dan tidak butuh banyak teman sejenis', _).
criteria_question(bisa_dilatih, yes_no, 'Apakah Anda ingin hewan yang relatif mudah dilatih', _).
criteria_question(membutuhkan_perhatian_ekstra, yes_no, 'Apakah Anda siap memberikan perhatian dan perawatan ekstra untuk hewan dengan kebutuhan khusus', _).
criteria_question(umur_panjang, yes_no, 'Apakah Anda mencari hewan dengan umur yang panjang (lebih dari 10 tahun)', _).

criteria_explanation_text(ukuran_tempat_tinggal_apartemen, 'Anda memilih tinggal di apartemen').
criteria_explanation_text(waktu_luang_sedikit, 'Waktu luang Anda sedikit (< 1 jam per hari)').
criteria_explanation_text(anggaran_bulanan_rendah, 'Anggaran bulanan Anda rendah (< Rp200.000 per bulan)').
criteria_explanation_text(interaksi_rendah, 'Anda menginginkan interaksi rendah dengan hewan').
criteria_explanation_text(komitmen_jangka_pendek, 'Anda siap untuk komitmen jangka pendek (1-3 tahun)').
criteria_explanation_text(ukuran_hewan_kecil, 'Anda menginginkan hewan berukuran kecil (seukuran hamster, ikan cupang)').
criteria_explanation_text(pengguna_adalah_pemula, 'Anda adalah seorang pemula dalam merawat hewan').
criteria_explanation_text(grooming_rendah, 'Anda bersedia melakukan grooming tingkat rendah (seminggu sekali atau kurang)').
criteria_explanation_text(toleransi_bising_rendah, 'Toleransi Anda terhadap kebisingan hewan rendah').
criteria_explanation_text(alergi_ringan, 'Ada anggota keluarga dengan alergi ringan (bersin, gatal ringan)').
criteria_explanation_text(sifat_estetik, 'Anda mempertimbangkan nilai estetika hewan').
criteria_explanation_text(ukuran_tempat_tinggal_rumah_kecil, 'Anda tinggal di rumah kecil (tipe 36-45)').
criteria_explanation_text(toleransi_bising_tinggi, 'Toleransi Anda terhadap kebisingan hewan tinggi').
criteria_explanation_text(anggaran_bulanan_sedang, 'Anggaran bulanan Anda sedang (Rp200.000 - Rp500.000 per bulan)').
criteria_explanation_text(interaksi_sedang, 'Anda menginginkan interaksi tingkat sedang dengan hewan').
criteria_explanation_text(ada_halaman, 'Anda memiliki halaman').
criteria_explanation_text(ada_anak_kecil, 'Ada anak kecil (di bawah 7 tahun) di rumah').
criteria_explanation_text(aktivitas_hewan_tenang, 'Anda lebih suka hewan yang tenang').
criteria_explanation_text(waktu_luang_sedang, 'Waktu luang Anda sedang (1-2 jam per hari)').
criteria_explanation_text(butuh_ruang_gerak_cukup, 'Anda dapat menyediakan ruang gerak yang cukup').
criteria_explanation_text(interaksi_tinggi, 'Anda menginginkan interaksi tinggi dengan hewan').
criteria_explanation_text(sifat_mandiri, 'Anda mencari hewan yang mandiri').
criteria_explanation_text(alergi_tidak_ada, 'Tidak ada anggota keluarga yang alergi bulu').
criteria_explanation_text(ukuran_tempat_tinggal_rumah_besar, 'Anda tinggal di rumah besar (> tipe 45 atau dengan halaman luas)').
criteria_explanation_text(waktu_luang_banyak, 'Waktu luang Anda banyak (> 2 jam per hari)').
criteria_explanation_text(anggaran_bulanan_tinggi, 'Anggaran bulanan Anda tinggi (> Rp500.000 per bulan)').
criteria_explanation_text(aktivitas_hewan_aktif, 'Anda lebih suka hewan yang aktif').
criteria_explanation_text(bukan_muslim, 'Anda bukan seorang Muslim (memungkinkan rekomendasi anjing)').
criteria_explanation_text(ukuran_hewan_besar, 'Anda menginginkan hewan berukuran besar (seukuran anjing golden retriever)').
criteria_explanation_text(ukuran_hewan_sedang, 'Anda menginginkan hewan berukuran sedang (seukuran kucing, anjing beagle)').
criteria_explanation_text(komitmen_jangka_panjang, 'Anda siap untuk komitmen jangka panjang (>8 tahun)').
criteria_explanation_text(pengguna_bukan_pemula, 'Anda bukan seorang pemula (berpengalaman)').
criteria_explanation_text(sifat_aktif_malam, 'Anda tidak masalah dengan hewan yang aktif di malam hari').
criteria_explanation_text(sifat_soliter, 'Anda mencari hewan yang soliter').
criteria_explanation_text(grooming_tinggi, 'Anda bersedia melakukan grooming tingkat tinggi (setiap hari atau butuh keahlian khusus)').
criteria_explanation_text(tidak_ada_halaman, 'Anda tidak memiliki halaman').
criteria_explanation_text(alergi_berat, 'Ada anggota keluarga dengan alergi berat (sesak napas, ruam parah)').
criteria_explanation_text(toleransi_bising_sedang, 'Toleransi Anda terhadap kebisingan hewan sedang').
criteria_explanation_text(tidak_ada_anak_kecil, 'Tidak ada anak kecil (di bawah 7 tahun) di rumah').
criteria_explanation_text(komitmen_jangka_menengah, 'Anda siap untuk komitmen jangka menengah (4-8 tahun)').
criteria_explanation_text(grooming_sedang, 'Anda bersedia melakukan grooming tingkat sedang (beberapa kali seminggu)').

go :-
    welcome_message,
    findall(recommendation(Pet, Justification),
            recommended_pet(Pet, Justification),
            AllRecommendations),
    process_recommendation(AllRecommendations),
    cleanup.

welcome_message :-
    nl,
    write('====================================================='), nl,
    write('Selamat Datang di Sistem Pakar Rekomendasi Hewan Peliharaan!'), nl,
    write('====================================================='), nl,
    write('Saya akan membantu Anda menemukan hewan peliharaan yang'), nl,
    write('cocok berdasarkan preferensi dan gaya hidup Anda.'), nl,
    write('Jawablah pertanyaan berikut dengan jujur ya!'), nl.

process_recommendation(RecommendationList) :-
    ( RecommendationList \== [] -> % Jika ada rekomendasi spesifik
        extract_pets_from_recommendations(RecommendationList, RawPetList),
        sort(RawPetList, UniquePetsSorted),
        ( length(UniquePetsSorted, L), L =< 3 ->
            get_recommendations_for_unique_pets(UniquePetsSorted, RecommendationList, DisplayRecs)
        ;   random_permutation(UniquePetsSorted, PermutedUniquePets),
            take_first_n(3, PermutedUniquePets, RandomUniquePets),
            get_recommendations_for_unique_pets(RandomUniquePets, RecommendationList, DisplayRecs)
        ),
        output_recommendation_with_justification(DisplayRecs)
    ; % Tidak ada rekomendasi spesifik, hasilkan rekomendasi fallback
        generate_fallback_recommendations(FallbackRecs),
        output_recommendation_with_justification(FallbackRecs)
    ).

generate_fallback_recommendations(FallbackRecs) :-
    findall(recommendation(Pet, Justification),
            fallback_pet_justification(Pet, _RuleID, Justification),
            AllFallbackPets),
    (   AllFallbackPets == [] ->
        FallbackRecs = [recommendation(ikan_cupang, ['Perawatan mudah dan cocok untuk pemula.',
                                                     'Tidak membutuhkan banyak tempat.',
                                                     'Merupakan hewan yang indah untuk diamati.'])]
    ;   length(AllFallbackPets, NumAllFallbacks),
        (   NumAllFallbacks =< 2 ->
            FallbackRecs = AllFallbackPets
        ;   random_permutation(AllFallbackPets, PermutedFallbacks),
            take_first_n(2, PermutedFallbacks, FallbackRecs)
        )
    ).


extract_pets_from_recommendations([], []).
extract_pets_from_recommendations([recommendation(Pet, _)|T], [Pet|PT]) :-
    extract_pets_from_recommendations(T, PT).

get_recommendations_for_unique_pets([], _, []).
get_recommendations_for_unique_pets([UniquePet|RestUnique], AllRecommendations, [FirstMatch|ResultRecs]) :-
    member(recommendation(UniquePet, Justification), AllRecommendations), !,
    FirstMatch = recommendation(UniquePet, Justification),
    get_recommendations_for_unique_pets(RestUnique, AllRecommendations, ResultRecs).
get_recommendations_for_unique_pets([_|RestUnique], AllRecommendations, ResultRecs) :-
    get_recommendations_for_unique_pets(RestUnique, AllRecommendations, ResultRecs).


take_first_n(0, _, []) :- !.
take_first_n(_, [], []) :- !.
take_first_n(N, [H|T], [H|Rest]) :-
    N > 0,
    N1 is N - 1,
    take_first_n(N1, T, Rest).

output_recommendation_with_justification(PetListWithJustification) :-
    nl,
    write('====================================================='), nl,
    write(' HASIL REKOMENDASI:'), nl,
    write('====================================================='), nl,
    ( PetListWithJustification == [] ->
        write('Saat ini sistem belum dapat memberikan rekomendasi spesifik.'), nl,
        write('Coba konsultasi dengan ahli hewan peliharaan atau toko hewan terdekat.'), nl
    ;
        write('Berdasarkan jawaban Anda, berikut adalah beberapa hewan '), nl,
        write('peliharaan yang mungkin cocok untuk Anda:'), nl, nl,
        display_pets_with_justification(PetListWithJustification)
    ),
    write('====================================================='), nl,
    write('PERHATIAN: Ini adalah rekomendasi awal. Lakukan riset'), nl,
    write('lebih mendalam tentang kebutuhan spesifik setiap hewan'), nl,
    write('sebelum membuat keputusan akhir.'), nl,
    write('====================================================='), nl.

display_pets_with_justification([]).
display_pets_with_justification([recommendation(Pet, Justification)|T]) :-
    format('* ~w~n', [Pet]),
    pet_description(Pet),
    ( Justification \== [], Justification \= [''] ->
        write('  Alasan direkomendasikan untuk Anda:'), nl,
        display_justification_points(Justification)
    ; true
    ),
    pet_care_notes(Pet), nl,
    display_pets_with_justification(T).

display_justification_points([]).
display_justification_points([Reason|Reasons]) :-
    format('    - ~w~n', [Reason]),
    display_justification_points(Reasons).

cleanup :-
    retractall(yes(_)),
    retractall(no(_)).

pet_description(hamster) :- write('  Deskripsi: Hewan pengerat kecil, aktif di malam hari, perawatannya relatif mudah.').
pet_description(ikan_cupang) :- write('  Deskripsi: Ikan air tawar yang cantik, tidak butuh ruang besar, cenderung soliter.').
pet_description(burung_kenari) :- write('  Deskripsi: Burung penyanyi yang indah, cocok untuk menghidupkan suasana rumah.').
pet_description(kelinci) :- write('  Deskripsi: Hewan sosial yang lembut, butuh ruang gerak dan diet khusus (hay).').
pet_description(kucing_domestik) :- write('  Deskripsi: Hewan independen namun penyayang, banyak variasi sifat, cocok untuk indoor.').
pet_description(anjing_golden_retriever) :- write('  Deskripsi: Anjing keluarga yang setia, ramah, cerdas, dan butuh banyak latihan.').
pet_description(anjing_beagle) :- write('  Deskripsi: Anjing pemburu yang ceria, penasaran, aktif, dan memiliki suara khas.').
pet_description(kura_kura) :- write('  Deskripsi: Reptil yang berumur panjang, relatif tenang, butuh perawatan spesifik untuk habitat.').
pet_description(burung_parkit) :- write('  Deskripsi: Burung kecil yang ceria dan bisa diajari bicara, cocok untuk interaksi sedang.').
pet_description(sugar_glider) :- write('  Deskripsi: Mamalia kecil nokturnal, sangat sosial, butuh perhatian dan diet khusus. Membutuhkan pemilik berpengalaman.').
pet_description(hamster_syrian) :- write('  Deskripsi: Hamster yang lebih besar, cocok untuk satu kandang, aktif di malam hari.').
pet_description(kucing_persia) :- write('  Deskripsi: Kucing berbulu panjang yang tenang, elegan, dan butuh perawatan bulu intensif.').
pet_description(anjing_pomeranian) :- write('  Deskripsi: Anjing kecil yang lincah, cerdas, berbulu lebat, dan bisa sangat vokal.').
pet_description(leopard_gecko) :- write('  Deskripsi: Kadal nokturnal yang relatif mudah dirawat untuk pemula reptil, tenang.').
pet_description(_) :- !.

pet_care_notes(hamster) :- write('  Catatan: Butuh kandang yang aman, makanan biji-bijian, dan mainan. Umur pendek (2-3 tahun).').
pet_care_notes(ikan_cupang) :- write('  Catatan: Butuh akuarium minimal 5 liter dengan air bersih, jangan digabung dengan cupang jantan lain.').
pet_care_notes(burung_kenari) :- write('  Catatan: Butuh sangkar yang luas, pakan berkualitas, dan kebersihan kandang terjaga.').
pet_care_notes(kelinci) :- write('  Catatan: Perlu kandang besar/area bermain, 80% dietnya hay, rentan stres.').
pet_care_notes(kucing_domestik) :- write('  Catatan: Butuh vaksinasi rutin, litter box bersih, mainan, dan interaksi. Umur bisa 15+ tahun.').
pet_care_notes(anjing_golden_retriever) :- write('  Catatan: Butuh latihan fisik dan mental harian, grooming rutin, rentan masalah sendi.').
pet_care_notes(anjing_beagle) :- write('  Catatan: Butuh latihan intensif, pagar yang aman (suka kabur), cenderung berisik.').
pet_care_notes(kura_kura) :- write('  Catatan: Butuh pencahayaan UVB dan pemanas, kolam yang bersih, serta diet sayuran dan pelet khusus.').
pet_care_notes(burung_parkit) :- write('  Catatan: Butuh sangkar cukup besar, mainan, dan interaksi rutin. Berikan pakan biji-bijian khusus burung.').
pet_care_notes(sugar_glider) :- write('  Catatan: Butuh kandang tinggi, diet khusus (buah, nektar, protein), interaksi sosial yang tinggi. Perawatan kompleks.').
pet_care_notes(hamster_syrian) :- write('  Catatan: Kandang harus besar (minimal 450 inci persegi), makanan biji-bijian, mainan kunyah.').
pet_care_notes(kucing_persia) :- write('  Catatan: Perlu disisir setiap hari untuk mencegah bulu kusut, rentan masalah pernapasan.').
pet_care_notes(anjing_pomeranian) :- write('  Catatan: Grooming rutin, rentan masalah gigi, butuh sosialisasi dini.').
pet_care_notes(leopard_gecko) :- write('  Catatan: Butuh terrarium dengan gradien suhu, pelembap, dan serangga hidup sebagai makanan.').
pet_care_notes(_) :- !.
